-- MySQL dump 10.15  Distrib 10.0.20-MariaDB, for Linux (x86_64)
--
-- Host: localhost    Database: hrelwa03
-- ------------------------------------------------------
-- Server version	10.0.20-MariaDB-log

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `feeds`
--

DROP TABLE IF EXISTS `feeds`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `feeds` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `url` varchar(255) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `url` (`url`),
  KEY `id` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `feeds`
--

LOCK TABLES `feeds` WRITE;
/*!40000 ALTER TABLE `feeds` DISABLE KEYS */;
INSERT INTO `feeds` VALUES (8,'http://www.xrel.to/releases-usrss.html?u=20470&s=ee663473a8da8a161902c908326ebe1c&favs=1');
/*!40000 ALTER TABLE `feeds` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `releases`
--

DROP TABLE IF EXISTS `releases`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `releases` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `feed` int(10) unsigned NOT NULL,
  `name` varchar(255) NOT NULL,
  `track` tinyint(1) NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_release_per_feed` (`feed`,`name`),
  KEY `id` (`id`,`name`)
) ENGINE=InnoDB AUTO_INCREMENT=228 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `releases`
--

LOCK TABLES `releases` WRITE;
/*!40000 ALTER TABLE `releases` DISABLE KEYS */;
INSERT INTO `releases` VALUES (222,8,'true detective s02e04 720p hdtv x264',1),(223,8,'true detective s02e04 1080p hdtv x264',1),(224,8,'suits s05e04 720p hdtv x264',1),(225,8,'suits s05e04 internal 720p hdtv x264',1),(226,8,'mr robot s01e04 720p hdtv x264',1),(227,8,'key and peele s05e02 720p hdtv x264',1);
/*!40000 ALTER TABLE `releases` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `torrents`
--

DROP TABLE IF EXISTS `torrents`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `torrents` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `rel` int(10) unsigned NOT NULL,
  `url` varchar(255) NOT NULL,
  `size` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_rel_url` (`rel`,`url`),
  KEY `id` (`id`,`url`)
) ENGINE=InnoDB AUTO_INCREMENT=679 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `torrents`
--

LOCK TABLES `torrents` WRITE;
/*!40000 ALTER TABLE `torrents` DISABLE KEYS */;
INSERT INTO `torrents` VALUES (641,222,'magnet:?xt=urn:btih:3336A103D9B912419EB4980569C453D7D81EFD21&dn=true+detective+s02e04+720p+hdtv+x264+0sec+glodls&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',1675803488),(642,222,'http://torcache.net/torrent/3336A103D9B912419EB4980569C453D7D81EFD21.torrent',1675803488),(643,222,'magnet:?xt=urn:btih:867EB78DA5B7741B79C5FF224D135092150FA164&dn=true+detective+s02e04+720p+hdtv+x264+0sec+b2ride&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',1675796160),(644,222,'http://torcache.net/torrent/867EB78DA5B7741B79C5FF224D135092150FA164.torrent',1675796160),(645,222,'magnet:?xt=urn:btih:15A9B515B37C4A97C2D0253CAF28A1EB5BA8DFF2&dn=true+detective+s02e04+720p+hdtv+x264+0sec+rartv&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',1675796194),(646,222,'http://torcache.net/torrent/15A9B515B37C4A97C2D0253CAF28A1EB5BA8DFF2.torrent',1675796194),(647,223,'magnet:?xt=urn:btih:9FB09C0E63AC7B582872FE6C0F3DFB9293B48192&dn=true+detective+s02e04+1080p+hdtv+x264+batv+ethd&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',4294967295),(648,223,'http://torcache.net/torrent/9FB09C0E63AC7B582872FE6C0F3DFB9293B48192.torrent',4294967295),(649,223,'magnet:?xt=urn:btih:BFE801C472311659E66E86C796F691B09D79C776&dn=true+detective+s02e04+1080p+hdtv+x264+batv+rarbg&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',4294967295),(650,223,'http://torcache.net/torrent/BFE801C472311659E66E86C796F691B09D79C776.torrent',4294967295),(651,224,'magnet:?xt=urn:btih:9C4D720154D6A4998FC169F3B27BE9100D7DC3E1&dn=suits+s05e04+720p+hdtv+x264+immerse+b2ride&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',879190259),(652,224,'http://torcache.net/torrent/9C4D720154D6A4998FC169F3B27BE9100D7DC3E1.torrent',879190259),(653,224,'magnet:?xt=urn:btih:EF6061E1A90E45272CB5F7E473CCDF04B26609C2&dn=suits+s05e04+720p+hdtv+x264+immerse+rarbg&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',879190293),(654,224,'http://torcache.net/torrent/EF6061E1A90E45272CB5F7E473CCDF04B26609C2.torrent',879190293),(655,224,'magnet:?xt=urn:btih:8BB149C8AD84AABEB283260A43463E26EE932951&dn=suits+s05e04+720p+hdtv+x264+immerse+ethd&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',879190431),(656,224,'http://torcache.net/torrent/8BB149C8AD84AABEB283260A43463E26EE932951.torrent',879190431),(657,225,'magnet:?xt=urn:btih:7AADCA97A57D4C45FE738E88FFC7B7093D1DC74C&dn=suits+s05e04+internal+720p+hdtv+x264+batv+ethd&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',942784974),(658,225,'http://torcache.net/torrent/7AADCA97A57D4C45FE738E88FFC7B7093D1DC74C.torrent',942784974),(659,225,'magnet:?xt=urn:btih:7BF613799165CE7D5913135D2E9441E990B832ED&dn=suits+s05e04+internal+720p+hdtv+x264+batv+b2ride&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',942784802),(660,225,'http://torcache.net/torrent/7BF613799165CE7D5913135D2E9441E990B832ED.torrent',942784802),(661,225,'magnet:?xt=urn:btih:6FEB185EB1A9238BB68B6313301962F0B3D31E90&dn=suits+s05e04+internal+720p+hdtv+x264+batv+rarbg&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',942784836),(662,225,'http://torcache.net/torrent/6FEB185EB1A9238BB68B6313301962F0B3D31E90.torrent',942784836),(663,226,'magnet:?xt=urn:btih:4937D0E327DF9E271479393EC76916C6C69A2569&dn=mr+robot+s01e04+720p+hdtv+x264&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',674615591),(664,226,'http://torcache.net/torrent/4937D0E327DF9E271479393EC76916C6C69A2569.torrent',674615591),(665,226,'magnet:?xt=urn:btih:D40664DB30FDBB53C3B218F715C6DA9D58C6E8B2&dn=mr+robot+s01e04+720p+hdtv+x264+vinky0999&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',267684396),(666,226,'http://torcache.net/torrent/D40664DB30FDBB53C3B218F715C6DA9D58C6E8B2.torrent',267684396),(667,226,'magnet:?xt=urn:btih:2E333B77C0A66CB6515A9DA00F0DC51BACB79E1E&dn=mr+robot+s01e04+720p+hdtv+x264+killers&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',759939470),(668,226,'http://torcache.net/torrent/2E333B77C0A66CB6515A9DA00F0DC51BACB79E1E.torrent',759939470),(669,226,'magnet:?xt=urn:btih:6E291800D6C8EFA10F8EACB7BCAE3E83C36B5666&dn=mr+robot+s01e04+720p+hdtv+x264+killers+rartv&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',759939388),(670,226,'http://torcache.net/torrent/6E291800D6C8EFA10F8EACB7BCAE3E83C36B5666.torrent',759939388),(671,226,'magnet:?xt=urn:btih:45B5708D9453CFD7084184B9F9CB667839E836B4&dn=mr+robot+s01e04+720p+hdtv+x264+killers+ethd&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',759939526),(672,226,'http://torcache.net/torrent/45B5708D9453CFD7084184B9F9CB667839E836B4.torrent',759939526),(673,226,'magnet:?xt=urn:btih:3A7222429FEC29E075833BFE1557D97C57C21DCD&dn=mr+robot+s01e04+720p+hdtv+x264+killers+b2ride&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',759939354),(674,226,'http://torcache.net/torrent/3A7222429FEC29E075833BFE1557D97C57C21DCD.torrent',759939354),(675,227,'magnet:?xt=urn:btih:D8C500C61FA11E308F3976A7524F3345B81E7C49&dn=key+and+peele+s05e02+720p+hdtv+x264+mindthegap+b2ride&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',513716700),(676,227,'http://torcache.net/torrent/D8C500C61FA11E308F3976A7524F3345B81E7C49.torrent',513716700),(677,227,'magnet:?xt=urn:btih:BDDD0BC7628FA699A67DFC351C220CCD2DF48A30&dn=key+and+peele+s05e02+720p+hdtv+x264+mindthegap+rartv&tr=udp%3A%2F%2Ftracker.publicbt.com%2Fannounce&tr=udp%3A%2F%2Fopen.demonii.com%3A1337',513716734),(678,227,'http://torcache.net/torrent/BDDD0BC7628FA699A67DFC351C220CCD2DF48A30.torrent',513716734);
/*!40000 ALTER TABLE `torrents` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-07-18  0:30:09
