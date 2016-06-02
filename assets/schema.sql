--
-- PostgreSQL database dump
--

-- Dumped from database version 9.5.2
-- Dumped by pg_dump version 9.5.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

--
-- Name: count_feed_links(integer); Type: FUNCTION; Schema: public; Owner: hrel
--

CREATE FUNCTION count_feed_links(id integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
SELECT COUNT(l.id) FROM feed_contents fc, links l WHERE fc.feed = $1 AND fc.release = l.release
$_$;


ALTER FUNCTION public.count_feed_links(id integer) OWNER TO hrel;

--
-- Name: count_feed_releases(integer); Type: FUNCTION; Schema: public; Owner: hrel
--

CREATE FUNCTION count_feed_releases(id integer) RETURNS bigint
    LANGUAGE sql
    AS $_$
SELECT COUNT(fc.release) FROM feed_contents fc WHERE fc.feed = $1
$_$;


ALTER FUNCTION public.count_feed_releases(id integer) OWNER TO hrel;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: feed_contents; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE feed_contents (
    feed integer NOT NULL,
    release integer NOT NULL
);


ALTER TABLE feed_contents OWNER TO hrel;

--
-- Name: feeds; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE feeds (
    id integer NOT NULL,
    uri character varying(255) NOT NULL,
    title character varying(255)
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
-- Name: links; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE links (
    id integer NOT NULL,
    title character varying(255) NOT NULL,
    uri character varying(255) NOT NULL,
    release integer NOT NULL,
    inserted timestamp with time zone DEFAULT now() NOT NULL,
    source character varying(255) NOT NULL
);


ALTER TABLE links OWNER TO hrel;

--
-- Name: links_id_seq; Type: SEQUENCE; Schema: public; Owner: hrel
--

CREATE SEQUENCE links_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE links_id_seq OWNER TO hrel;

--
-- Name: links_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: hrel
--

ALTER SEQUENCE links_id_seq OWNED BY links.id;


--
-- Name: releases; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE releases (
    id integer NOT NULL,
    name character varying(255) NOT NULL,
    type character varying(31) DEFAULT 'unknown'::character varying
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
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds ALTER COLUMN id SET DEFAULT nextval('feeds_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY links ALTER COLUMN id SET DEFAULT nextval('links_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY releases ALTER COLUMN id SET DEFAULT nextval('releases_id_seq'::regclass);


--
-- Name: feed_contents_feed_release_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feed_contents
    ADD CONSTRAINT feed_contents_feed_release_key UNIQUE (feed, release);


--
-- Name: feeds_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds
    ADD CONSTRAINT feeds_pkey PRIMARY KEY (id);


--
-- Name: feeds_uri_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds
    ADD CONSTRAINT feeds_uri_key UNIQUE (uri);


--
-- Name: links_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY links
    ADD CONSTRAINT links_pkey PRIMARY KEY (id);


--
-- Name: links_uri_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY links
    ADD CONSTRAINT links_uri_key UNIQUE (uri);


--
-- Name: releases_name_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY releases
    ADD CONSTRAINT releases_name_key UNIQUE (name);


--
-- Name: releases_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY releases
    ADD CONSTRAINT releases_pkey PRIMARY KEY (id);


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
-- Name: links_release_fkey; Type: FK CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY links
    ADD CONSTRAINT links_release_fkey FOREIGN KEY (release) REFERENCES releases(id);


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

