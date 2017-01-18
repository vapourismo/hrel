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

SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: feed_contents; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE feed_contents (
    feed bigint NOT NULL,
    title character varying NOT NULL
);


ALTER TABLE feed_contents OWNER TO hrel;

--
-- Name: feeds; Type: TABLE; Schema: public; Owner: hrel
--

CREATE TABLE feeds (
    id bigint NOT NULL,
    title character varying DEFAULT ''::character varying NOT NULL,
    url character varying NOT NULL
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
-- Name: feeds id; Type: DEFAULT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds ALTER COLUMN id SET DEFAULT nextval('feeds_id_seq'::regclass);


--
-- Name: feed_contents feed_contents_title_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feed_contents
    ADD CONSTRAINT feed_contents_title_key UNIQUE (title);


--
-- Name: feeds feeds_pkey; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds
    ADD CONSTRAINT feeds_pkey PRIMARY KEY (id);


--
-- Name: feeds feeds_url_key; Type: CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feeds
    ADD CONSTRAINT feeds_url_key UNIQUE (url);


--
-- Name: feed_contents feed_contents_feed_fkey; Type: FK CONSTRAINT; Schema: public; Owner: hrel
--

ALTER TABLE ONLY feed_contents
    ADD CONSTRAINT feed_contents_feed_fkey FOREIGN KEY (feed) REFERENCES feeds(id);


--
-- PostgreSQL database dump complete
--

