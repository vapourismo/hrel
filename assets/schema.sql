--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: dumps; Type: TABLE; Schema: public; Owner: hrel; Tablespace: 
--

CREATE TABLE dumps (
    id integer NOT NULL,
    uri character varying(255) NOT NULL,
    type character varying(255) NOT NULL
);


ALTER TABLE dumps OWNER TO hrel;

--
-- Name: dumps_id_seq; Type: SEQUENCE; Schema: public; Owner: hrel
--

CREATE SEQUENCE dumps_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE dumps_id_seq OWNER TO hrel;

--
-- Name: dumps_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hrel
--

ALTER SEQUENCE dumps_id_seq OWNED BY dumps.id;


--
-- Name: feed_contents; Type: TABLE; Schema: public; Owner: hrel; Tablespace: 
--

CREATE TABLE feed_contents (
    feed integer NOT NULL,
    release integer NOT NULL
);


ALTER TABLE feed_contents OWNER TO hrel;

--
-- Name: feeds; Type: TABLE; Schema: public; Owner: hrel; Tablespace: 
--

CREATE TABLE feeds (
    id integer NOT NULL,
    uri character varying(255) NOT NULL
);


ALTER TABLE feeds OWNER TO hrel;

--
-- Name: feeds_id_seq; Type: SEQUENCE; Schema: public; Owner: hrel
--

CREATE SEQUENCE feeds_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE feeds_id_seq OWNER TO hrel;

--
-- Name: feeds_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hrel
--

ALTER SEQUENCE feeds_id_seq OWNED BY feeds.id;


--
-- Name: releases; Type: TABLE; Schema: public; Owner: hrel; Tablespace: 
--

CREATE TABLE releases (
    id integer NOT NULL,
    name character varying(255) NOT NULL
);


ALTER TABLE releases OWNER TO hrel;

--
-- Name: releases_id_seq; Type: SEQUENCE; Schema: public; Owner: hrel
--

CREATE SEQUENCE releases_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE releases_id_seq OWNER TO hrel;

--
-- Name: releases_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hrel
--

ALTER SEQUENCE releases_id_seq OWNED BY releases.id;


--
-- Name: torrents; Type: TABLE; Schema: public; Owner: hrel; Tablespace: 
--

CREATE TABLE torrents (
    id integer NOT NULL,
    title character varying(255) NOT NULL,
    uri character varying(255) NOT NULL,
    release integer NOT NULL,
    size bigint NOT NULL
);


ALTER TABLE torrents OWNER TO hrel;

--
-- Name: torrents_id_seq; Type: SEQUENCE; Schema: public; Owner: hrel
--

CREATE SEQUENCE torrents_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE torrents_id_seq OWNER TO hrel;

--
-- Name: torrents_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hrel
--

ALTER SEQUENCE torrents_id_seq OWNED BY torrents.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY dumps ALTER COLUMN id SET DEFAULT nextval('dumps_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds ALTER COLUMN id SET DEFAULT nextval('feeds_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY releases ALTER COLUMN id SET DEFAULT nextval('releases_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY torrents ALTER COLUMN id SET DEFAULT nextval('torrents_id_seq'::regclass);


--
-- Name: dumps_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY dumps
    ADD CONSTRAINT dumps_pkey PRIMARY KEY (id);


--
-- Name: dumps_uri_key; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY dumps
    ADD CONSTRAINT dumps_uri_key UNIQUE (uri);


--
-- Name: feed_contents_feed_release_key; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY feed_contents
    ADD CONSTRAINT feed_contents_feed_release_key UNIQUE (feed, release);


--
-- Name: feeds_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY feeds
    ADD CONSTRAINT feeds_pkey PRIMARY KEY (id);


--
-- Name: feeds_uri_key; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY feeds
    ADD CONSTRAINT feeds_uri_key UNIQUE (uri);


--
-- Name: releases_name_key; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY releases
    ADD CONSTRAINT releases_name_key UNIQUE (name);


--
-- Name: releases_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY releases
    ADD CONSTRAINT releases_pkey PRIMARY KEY (id);


--
-- Name: torrents_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY torrents
    ADD CONSTRAINT torrents_pkey PRIMARY KEY (id);


--
-- Name: torrents_uri_key; Type: CONSTRAINT; Schema: public; Owner: hrel; Tablespace: 
--

ALTER TABLE ONLY torrents
    ADD CONSTRAINT torrents_uri_key UNIQUE (uri);


--
-- Name: feed_contents_feed_fkey; Type: FK CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feed_contents
    ADD CONSTRAINT feed_contents_feed_fkey FOREIGN KEY (feed) REFERENCES feeds(id);


--
-- Name: feed_contents_release_fkey; Type: FK CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feed_contents
    ADD CONSTRAINT feed_contents_release_fkey FOREIGN KEY (release) REFERENCES releases(id);


--
-- Name: torrents_release_fkey; Type: FK CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY torrents
    ADD CONSTRAINT torrents_release_fkey FOREIGN KEY (release) REFERENCES releases(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

