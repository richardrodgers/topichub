/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package store

import java.io.{File, FileInputStream, FileOutputStream, InputStream}
import java.security.{DigestInputStream, MessageDigest, NoSuchAlgorithmException}
import java.util.zip.{ZipFile}

import org.joda.time.format.ISODateTimeFormat

import play._
import play.api.Configuration
import play.mvc._

import models._
import controllers._

/**
 * These traits, classes and objects abstract the details of item content storage
 * The current implementation uses a local file location, but remote S3 storage, etc
 * might make more sense ultimately. Content files are stored in a shallow directory
 * tree, with the naming convention
 *     xx/xxyyyyyyy.zip
 * where xx represents the first two characters of the MD5 of the file,
 * and xxyyyyy is the entire checksum
 */

trait StoredContent {

  def content: InputStream
  def resource(name: String): InputStream
  def md5: String
  def mimetype: String
}

case class LocalZipFile(pkg: File, md5: String) extends StoredContent {

  val zip = new ZipFile(pkg)

  override def content = new FileInputStream(pkg)

  override def resource(name: String): InputStream  = {
    val entry = zip.getEntry(name)
    if (entry != null) zip.getInputStream(entry) else null
  }

  override def mimetype = "application/zip"
}

object Store {
  //val store = Configuration.load(new File("configuration.cfg")).getString("thub.item.store").getOrElse("")
  val store = "/Users/richardrodgers/thub-test"
    //def index = Template

  def upload(tempFile: File, maxSize: Int, md5: String): Either[SwordError.Value, StoredContent] = {
    // create temp upload file location
    var tmpFile = File.createTempFile("collId", ".zip", new File(store))
    val fout = new FileOutputStream(tmpFile)
    var count = 0
    var buf = new Array[Byte](4096)
    val din = new DigestInputStream(new FileInputStream(tempFile), MessageDigest.getInstance("MD5"))
    var read = 0
    var quit = false
    while (read != -1 && ! quit) {
      read = din.read(buf)
      if (read > 0)  {
        count += read
        if (maxSize > -1 && count > maxSize)  {
          // bail out
          quit = true
        }
        fout.write(buf, 0, read)
      }
    }
    val ulMd5 = toHex(din.getMessageDigest().digest())
    din.close
    fout.close
    //tempFile.close
    if (quit) {
      // blow away partial upload, and return error
      tmpFile.delete()
      Left(SwordError.MaxUploadSizeExceeded)
    } else if (md5 != null && ! md5.equals(ulMd5)) {
      // now make sure checksums agree
      tmpFile.delete()
      Left(SwordError.ErrorChecksumMismatch)
    } else {
      val subDir = new File(store, ulMd5.substring(0, 2))
      subDir.mkdirs()
      val permFile = new File(subDir, ulMd5 + ".zip")
      if (permFile.exists) {
        tmpFile.delete()
        Left(SwordError.DuplicateContent)
      } else {
        // move temp file to proper location, and return it
        if (! subDir.exists) subDir.mkdir
        tmpFile.renameTo(permFile)
        Right(LocalZipFile(permFile, ulMd5))
      }
    }
  }

  def content(item: Item) = {
    val subDir = new File(store, item.itemId.substring(0, 2))
    val file = new File(subDir, item.itemId + ".zip")
    LocalZipFile(file, item.itemId)
  }

  def listContents = {
    var list: List[StoredContent] = List()
    val dirs = new File(store).listFiles
    for (dir <- dirs) {
      for (file <- dir.listFiles) {
        val name = file.getName
        val md5 = name.substring(0, name.indexOf("."))
        list = new LocalZipFile(new File(dir, name), md5) :: list
      }
    }
    list
  }

  private def toHex(bytes: Array[Byte]): String =
     bytes.map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    
}
