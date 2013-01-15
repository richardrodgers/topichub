/**
 * Copyright 2012 MIT Libraries
 * Licensed under: http://www.apache.org/licenses/LICENSE-2.0
 */
package store

import java.io.{File, FileInputStream, FileOutputStream, InputStream}
import java.security.{DigestInputStream, MessageDigest, NoSuchAlgorithmException}
import java.util.zip.{ZipFile, ZipEntry, ZipInputStream}

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.{AmazonS3, AmazonS3Client}
import com.amazonaws.services.s3.model.{S3Object, GetObjectRequest}

import play.api._
import play.api.libs.Files.TemporaryFile
import play.api.Play.current

import models.Item
import controllers.SwordError

/** Traits, classes and objects to abstract details of item content storage.
  * Current implementations: local file system or remote Amazon S3 storage
  * TODO: refactor for real plug-in modularity
  *
  * @author richardrodgers
  */

trait ContentStore {
  def contains(md5: String): Boolean
  def store(file: File, md5: String): Unit
  def retrieve(md5: String): StoredContent
}

trait StoredContent {
  def content: InputStream
  def resource(name: String): InputStream
  def md5: String
  def mimetype: String = "application/zip"
}

/**
  * Storage in a managed directory in the local file system.
  * Files are stored in a shallow directory tree, with naming convention
  *     xx/xxyyyyyyy.zip
  * where xx represents the first two characters of the MD5 of the file,
  * and xxyyyyy is the entire checksum
  */
object LocalStore extends ContentStore {
  // Assumes format 'file:///directory/path'
  val rootDir = Play.configuration.getString("hub.item.store").get.substring(7)

  override def contains(md5: String) = storedFile(md5).exists

  // optimizaton - rename, rather than copy, file
  override def store(file: File, md5: String) = file.renameTo(storedFile(md5, true))

  override def retrieve(md5: String) = LocalZipFile(storedFile(md5), md5)

  private def storedFile(md5: String, make: Boolean = false) = {
    val subDir = new File(rootDir, md5.substring(0, 2))
    if (make) subDir.mkdirs
    new File(subDir, md5 + ".zip")
  }
}

case class LocalZipFile(pkg: File, md5: String) extends StoredContent {

  val zip = new ZipFile(pkg)

  override def content = new FileInputStream(pkg)

  override def resource(name: String): InputStream  = {
    val entry = zip.getEntry(name)
    if (entry != null) zip.getInputStream(entry) else null
  }
}

/**
  * Amazon S3 storage.
  * Files are all stored with object IDs = file md5s in a single bucket
  * configured via the URL in the 'hub.item.store' property.
  * Bucket must be created before use.
  */
object S3Store extends ContentStore {
  // Assumes format 'http://bucketname.s3.amazonaws.com'
  val url = Play.configuration.getString("hub.item.store").get
  val bucket = url.substring(7, url.indexOf(".s3.amazonaws.com"))
  val accessKey = Play.configuration.getString("hub.aws.accesskey").get
  val secretKey = Play.configuration.getString("hub.aws.secretkey").get
  val s3 = new AmazonS3Client(new BasicAWSCredentials(accessKey, secretKey))

  override def contains(md5: String) = {
    try {
      s3.getObjectMetadata(bucket, md5)
      true
    } catch {
      case _ => false
    }
  }

  override def store(file: File, md5: String) {
    s3.putObject(bucket, md5, file)
    // remove local temp file
    file.delete
  }

  override def retrieve(md5: String) = {
    // defer actual retrieval until needed - to ensure http connections to S3 are conserved
    S3ZipFile(s3, bucket, md5)
  }
}


case class S3ZipFile(s3: AmazonS3, bucket: String, md5: String) extends StoredContent {

  override def content = {
    val s3o = s3.getObject(new GetObjectRequest(bucket, md5))
    s3o.getObjectContent
  }

  override def resource(name: String): InputStream = {
    val s3o = s3.getObject(new GetObjectRequest(bucket, md5))
    // filter entries until name found
    val zin = new ZipInputStream(s3o.getObjectContent)
    var entry: ZipEntry = null
    while((entry = zin.getNextEntry) != null) {
      if (entry.getName.equals(name)) {
        return zin
      }
    }
    null
  }
}

object Store {
  val storeCfg = Play.configuration.getString("hub.item.store").get
  // crude test for now, only 2 backends supported
  val cstore = if (storeCfg.startsWith("file")) LocalStore else S3Store

  def upload(tempFile: TemporaryFile, maxSize: Int, md5: String): Either[SwordError.Value, StoredContent] = {
    // create temp upload file location
    var tmpFile = File.createTempFile("collId", ".zip")
    val fout = new FileOutputStream(tmpFile)
    var count = 0
    var buf = new Array[Byte](4096)
    val din = new DigestInputStream(new FileInputStream(tempFile.file), MessageDigest.getInstance("MD5"))
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
    } else if (cstore.contains(ulMd5)) {
      // reject as duplicate
      tmpFile.delete()
      Left(SwordError.DuplicateContent)
    } else {
      // save file
      cstore.store(tmpFile, ulMd5)
      Right(cstore.retrieve(ulMd5))
    }
  }

  def content(item: Item) = cstore.retrieve(item.itemId)

  // TODO
  // def listContents: List[StoredContent]

  private def toHex(bytes: Array[Byte]): String =
     bytes.map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    
}
