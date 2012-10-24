# core model types
# --- !Ups

CREATE SEQUENCE publisher_id_seq;
CREATE TABLE publisher (
	id integer NOT NULL DEFAULT nextval('publisher_id_seq'),
	pubId varchar UNIQUE,
	name varchar,
	description varchar,
	role varchar,
	home varchar,
	logo varchar,
	created timestamp,
	PRIMARY KEY(id)
);

CREATE SEQUENCE ctype_id_seq;
CREATE TABLE ctype (
	id integer NOT NULL DEFAULT nextval('ctype_id_seq'),
	ctypeId varchar UNIQUE,
	description varchar,
	logo varchar,
	PRIMARY KEY(id)
);

CREATE SEQUENCE pkgmap_id_seq;
CREATE TABLE pkgmap (
	id integer NOT NULL DEFAULT nextval('pkgmap_id_seq'),
	pkgmapId varchar UNIQUE,
	description varchar,
	swordurl varchar,
	PRIMARY KEY(id)
);

CREATE SEQUENCE collection_id_seq;
CREATE TABLE collection (
	id integer NOT NULL DEFAULT nextval('collection_id_seq'),
	publisher_id integer,
	ctype_id integer,
	pkgmap_id integer,
	description varchar,
	policy varchar,
  created timestamp,
  updated timestamp,
  deposits integer,
	FOREIGN KEY(publisher_id) REFERENCES publisher(id),
	FOREIGN KEY(ctype_id) REFERENCES ctype(id),
	FOREIGN KEY(pkgmap_id) REFERENCES pkgmap(id),
	PRIMARY KEY(id)
);

CREATE SEQUENCE scheme_id_seq;
CREATE TABLE scheme (
	id integer NOT NULL DEFAULT nextval('scheme_id_seq'),
	schemeId varchar(255) UNIQUE,
	gentype varchar,
	category varchar,
	description varchar,
	home varchar,
	logo varchar,
	created timestamp,
	PRIMARY KEY(id)
);

CREATE SEQUENCE ctypescheme_id_seq;
CREATE TABLE ctypescheme (
	id integer NOT NULL DEFAULT nextval('ctypescheme_id_seq'),
	ctype_id integer,
	scheme_id integer,
	relation varchar,
	FOREIGN KEY(ctype_id) REFERENCES ctype(id),
	FOREIGN KEY(scheme_id) REFERENCES scheme(id),
	PRIMARY KEY(id)
);

CREATE SEQUENCE pkgmapscheme_id_seq;
CREATE TABLE pkgmapscheme (
	id integer NOT NULL DEFAULT nextval('pkgmapscheme_id_seq'),
	pkgmap_id integer,
	scheme_id integer,
	source varchar(255) NOT NULL,
	format varchar NOT NULL,
	rank integer,
	FOREIGN KEY(pkgmap_id) REFERENCES pkgmap(id),
	FOREIGN KEY(scheme_id) REFERENCES scheme(id),
	PRIMARY KEY(id)
);

CREATE SEQUENCE topic_id_seq;
CREATE TABLE topic (
	id integer NOT NULL DEFAULT nextval('topic_id_seq'),
	scheme_id integer,
	topicId varchar(255),
	title varchar,
	created timestamp,
	updated timestamp,
	transfers integer,
	FOREIGN KEY(scheme_id) REFERENCES scheme(id),
	PRIMARY KEY(id)
);

CREATE SEQUENCE item_id_seq;
CREATE TABLE item (
	id integer NOT NULL DEFAULT nextval('item_id_seq'),
	collection_id integer,
	ctype_id integer,
	itemId varchar(255) NOT NULL,
  created timestamp,
  updated timestamp,
  transfers integer,
  FOREIGN KEY(collection_id) REFERENCES collection(id),
  FOREIGN KEY(ctype_id) REFERENCES ctype(id),
  PRIMARY KEY(id)
);

CREATE SEQUENCE metadata_id_seq;
CREATE TABLE metadata (
	id integer NOT NULL DEFAULT nextval('metadata_id_seq'),
	item_id integer,
	mdname varchar,
	mdvalue varchar,
	FOREIGN KEY(item_id) REFERENCES item(id),
	PRIMARY KEY(id)
);

CREATE SEQUENCE itemtopic_id_seq;
CREATE TABLE itemtopic (
	id integer NOT NULL DEFAULT nextval('itemtopic_id_seq'),
	item_id integer,
	item_created timestamp,
	topic_id integer,
	FOREIGN KEY(item_id) REFERENCES item(id),
	FOREIGN KEY(topic_id) REFERENCES topic(id),
	PRIMARY KEY(id)
);

CREATE SEQUENCE finder_id_seq;
CREATE TABLE finder (
	id integer NOT NULL DEFAULT nextval('finder_id_seq'),
	scheme_id integer,
  description varchar(255) NOT NULL,
  cardinality varchar(255) NOT NULL,
  format varchar(255) NOT NULL,
  idKey varchar(255) NOT NULL,
  idLabel varchar(255) NOT NULL,
  author varchar,
  created timestamp,
  FOREIGN KEY(scheme_id) REFERENCES scheme(id),
  PRIMARY KEY (id)
);

CREATE SEQUENCE subscriber_id_seq;
CREATE TABLE subscriber (
  id integer NOT NULL DEFAULT nextval('subscriber_id_seq'),
  userId varchar(255) NOT NULL,
  password varchar(255) NOT NULL,
  home varchar,
  logo varchar,
  role varchar,
  contact varchar(255),
  swordService varchar(255),
  terms varchar(255),
  backFile varchar(255),
  created timestamp,
  PRIMARY KEY (id)
);

CREATE SEQUENCE target_id_seq;
CREATE TABLE target (
  id integer NOT NULL DEFAULT nextval('target_id_seq'),
  subscriber_id integer,
  description varchar(255) NOT NULL,
  userId varchar(255) NOT NULL,
  password varchar(255) NOT NULL,
  depositUrl varchar(255) NOT NULL,
  created timestamp,
  updated timestamp,
  transfers integer,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  PRIMARY KEY (id)
);

CREATE SEQUENCE subscription_id_seq;
CREATE TABLE subscription (
  id integer NOT NULL DEFAULT nextval('subscription_id_seq'),
  subscriber_id integer,
  target_id integer,
  topic_id integer,
  policy varchar(255),
  created timestamp,
  updated timestamp,
  transfers integer,
  FOREIGN KEY(subscriber_id) REFERENCES subscriber(id),
  FOREIGN KEY(target_id) REFERENCES target(id),
  FOREIGN KEY(topic_id) REFERENCES topic(id),
  PRIMARY KEY (id)
);

CREATE SEQUENCE transfer_id_seq;
CREATE TABLE transfer (
  id integer NOT NULL DEFAULT nextval('transfer_id_seq'),
  target_id integer,
  subscription_id integer,
  itemId varchar(255) NOT NULL,
  target_addr varchar,
  created timestamp,
  state varchar(255),
  modified timestamp,
  FOREIGN KEY(target_id) REFERENCES target(id),
  FOREIGN KEY(subscription_id) REFERENCES subscription(id),
  PRIMARY KEY (id)
);

# --- !Downs

DROP TABLE metadata;
DROP SEQUENCE metadata_id_seq;
DROP TABLE finder;
DROP SEQUENCE finder_id_seq;
DROP TABLE itemtopic;
DROP SEQUENCE itemtopic_id_seq;
DROP TABLE ctypescheme;
DROP SEQUENCE ctypescheme_id_seq;
DROP TABLE pkgmapscheme;
DROP SEQUENCE pkgmapscheme_id_seq;
DROP TABLE topic;
DROP SEQUENCE topic_id_seq;
DROP TABLE scheme;
DROP SEQUENCE scheme_id_seq;
DROP TABLE item;
DROP SEQUENCE item_id_seq;
DROP TABLE collection;
DROP SEQUENCE collection_id_seq;
DROP TABLE ctype;
DROP SEQUENCE ctype_id_seq;
DROP TABLE pgkmap;
DROP SEQUENCE pkgmap_id_seq;
DROP TABLE publisher;
DROP SEQUENCE publisher_id_seq;
DROP TABLE transfer;
DROP SEQUENCE transfer_id_seq;
DROP TABLE subscription;
DROP SEQUENCE subscription_id_seq;
DROP TABLE target;
DROP SEQUENCE target_id_seq;
DROP TABLE subscriber;
DROP SEQUENCE subscriber_id_seq;
