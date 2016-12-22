--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.1
-- Dumped by pg_dump version 9.6.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
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

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: tags; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE tags (
    torrent bigint NOT NULL,
    tag character varying NOT NULL
);


ALTER TABLE tags OWNER TO hrel;

--
-- Name: torrents; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE torrents (
    id bigint NOT NULL,
    title character varying NOT NULL,
    uri character varying NOT NULL,
    insertion timestamp with time zone DEFAULT now() NOT NULL
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
-- Name: torrents id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY torrents ALTER COLUMN id SET DEFAULT nextval('torrents_id_seq'::regclass);


--
-- Name: tags tags_torrent_tag_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY tags
    ADD CONSTRAINT tags_torrent_tag_key UNIQUE (torrent, tag);


--
-- Name: torrents torrents_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY torrents
    ADD CONSTRAINT torrents_pkey PRIMARY KEY (id);


--
-- Name: torrents torrents_title_uri_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY torrents
    ADD CONSTRAINT torrents_title_uri_key UNIQUE (title, uri);


--
-- Name: tags tags_torrent_fkey; Type: FK CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY tags
    ADD CONSTRAINT tags_torrent_fkey FOREIGN KEY (torrent) REFERENCES torrents(id);


--
-- PostgreSQL database dump complete
--

