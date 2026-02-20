-- DVD Rental schema (generated-app style)
-- Run after creating database: psql -d dvdrental -f schema.sql

-- Reference
CREATE TABLE IF NOT EXISTS languages (
  id INT PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS categories (
  id INT PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS actors (
  id INT PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL
);

-- Films
CREATE TABLE IF NOT EXISTS films (
  id INT PRIMARY KEY,
  title TEXT NOT NULL,
  description TEXT,
  year INT,
  language_id INT REFERENCES languages(id),
  rental_duration INT,
  rental_rate NUMERIC(4,2),
  length INT,
  replacement_cost NUMERIC(5,2),
  rating TEXT
);

CREATE TABLE IF NOT EXISTS film_actors (
  film_id INT REFERENCES films(id),
  actor_id INT REFERENCES actors(id),
  PRIMARY KEY (film_id, actor_id)
);

CREATE TABLE IF NOT EXISTS film_categories (
  film_id INT REFERENCES films(id),
  category_id INT REFERENCES categories(id),
  PRIMARY KEY (film_id, category_id)
);

-- Customers & inventory
CREATE TABLE IF NOT EXISTS customers (
  id INT PRIMARY KEY,
  first_name TEXT NOT NULL,
  last_name TEXT NOT NULL,
  email TEXT NOT NULL,
  active BOOLEAN DEFAULT true
);

CREATE TABLE IF NOT EXISTS inventory (
  id INT PRIMARY KEY,
  film_id INT REFERENCES films(id),
  store_id INT DEFAULT 1
);

CREATE TABLE IF NOT EXISTS rentals (
  id INT PRIMARY KEY,
  rental_date TEXT NOT NULL,
  inventory_id INT REFERENCES inventory(id),
  customer_id INT REFERENCES customers(id),
  return_date TEXT
);

-- Seed reference data
INSERT INTO languages (id, name) VALUES
  (1, 'English'), (2, 'Italian'), (3, 'Japanese'), (4, 'French'), (5, 'German')
ON CONFLICT (id) DO NOTHING;

INSERT INTO categories (id, name) VALUES
  (1, 'Action'), (2, 'Animation'), (3, 'Comedy'), (4, 'Drama'),
  (5, 'Horror'), (6, 'Sci-Fi'), (7, 'Documentary'), (8, 'Thriller')
ON CONFLICT (id) DO NOTHING;

-- Seed actors (sample)
INSERT INTO actors (id, first_name, last_name) VALUES
  (1,'Alex','Sterling'), (2,'Maria','Vasquez'), (3,'David','Chen'),
  (4,'Sarah','Mitchell'), (5,'Marco','Bellini'), (6,'Yuki','Tanaka'),
  (7,'Emma','Thornton'), (8,'James','O''Connor'), (9,'Lisa','Park'),
  (10,'Robert','Hayes'), (11,'Anna','Kowalski'), (12,'Tom','Nakamura'),
  (13,'Claire','Dubois'), (14,'Michael','Reed'), (15,'Sofia','Reyes')
ON CONFLICT (id) DO NOTHING;

-- Seed films (sample)
INSERT INTO films (id, title, description, year, language_id, rental_duration, rental_rate, length, replacement_cost, rating) VALUES
  (1,'The Last Horizon','A team of astronauts ventures beyond the known galaxy.',2020,1,5,3.99,142,24.99,'PG-13'),
  (2,'Midnight in Rome','Two strangers meet on a rainy Roman night.',2019,2,3,2.99,118,19.99,'R'),
  (3,'Thunder Road','A retired special forces operative is pulled back into action.',2021,1,4,4.99,130,22.99,'PG-13'),
  (4,'The Laughing Fox','A cunning trickster outwits a corrupt mayor.',2022,1,3,2.99,98,17.99,'PG'),
  (5,'Shadows of the Deep','Deep-sea researchers encounter ancient horrors.',2020,1,5,3.99,105,21.99,'R')
ON CONFLICT (id) DO NOTHING;

-- More films 6-20 (abbreviated)
INSERT INTO films (id, title, description, year, language_id, rental_duration, rental_rate, length, replacement_cost, rating) VALUES
  (6,'Planet Zephyr','Young colonists on an alien world.',2023,1,5,4.99,136,24.99,'PG'),
  (7,'A Walk in the Rain','An aging professor reflects on his life.',2018,4,3,2.99,112,18.99,'PG-13'),
  (8,'Iron Fist Rising','Underground fighters compete.',2022,1,4,4.99,125,22.99,'R'),
  (9,'The Great Escape Artist','A bumbling magician in espionage.',2021,1,3,2.99,104,17.99,'PG'),
  (10,'Whispers in the Dark','A detective investigates cryptic messages.',2019,1,5,3.99,115,21.99,'R'),
  (11,'Ocean''s Fury','Navy divers face a rogue submarine.',2023,1,4,4.99,128,23.99,'PG-13'),
  (12,'Love in Tokyo','American photographer and Japanese chef.',2020,3,3,2.99,110,18.99,'PG-13'),
  (13,'The Robot''s Dream','A robot who learns to dream.',2022,1,3,2.99,95,16.99,'PG'),
  (14,'Night Terrors','A family in a house with a dark past.',2021,1,5,3.99,99,20.99,'R'),
  (15,'Funny Business','A comedian exposes money laundering.',2023,1,3,2.99,102,17.99,'PG-13'),
  (16,'Arctic Survival','Documentary: researchers stranded.',2020,1,4,3.99,88,19.99,'PG'),
  (17,'Dragon''s Keep','Animated epic about the last dragon.',2019,1,3,2.99,108,18.99,'PG'),
  (18,'The Silent Witness','A mute witness must communicate.',2022,1,5,4.99,119,22.99,'R'),
  (19,'Summer Daze','Four friends, one last vacation.',2018,1,3,1.99,96,15.99,'PG'),
  (20,'Galactic Pioneers','First colony ship faces mutiny.',2024,1,5,4.99,148,25.99,'PG-13')
ON CONFLICT (id) DO NOTHING;

-- Film-actor and film-category links
INSERT INTO film_actors (film_id, actor_id) VALUES
  (1,1),(1,3),(1,6),(2,2),(2,5),(2,13),(3,1),(3,8),(3,10),(4,7),(4,9),(4,15),(5,3),(5,4),(5,14),
  (6,6),(6,11),(6,12),(7,5),(7,13),(7,7),(8,1),(8,10),(8,14),(9,8),(9,9),(9,15),(10,4),(10,10),(10,3),
  (11,1),(11,8),(11,12),(12,2),(12,6),(12,9),(13,7),(13,11),(14,4),(14,14),(14,3),(15,9),(15,15),(15,8),
  (16,10),(16,13),(17,11),(17,12),(17,7),(18,2),(18,4),(18,14),(19,5),(19,7),(19,15),(20,1),(20,3),(20,6);
INSERT INTO film_categories (film_id, category_id) VALUES
  (1,6),(2,4),(3,1),(4,3),(5,5),(6,6),(7,4),(8,1),(9,3),(10,8),(11,1),(12,4),(13,2),(13,6),(14,5),(15,3),(16,7),(17,2),(17,1),(18,8),(19,3),(20,6);

-- Customers
INSERT INTO customers (id, first_name, last_name, email, active) VALUES
  (1,'John','Smith','john.smith@email.com',true), (2,'Jane','Doe','jane.doe@email.com',true),
  (3,'Bob','Johnson','bob.j@email.com',true), (4,'Alice','Brown','alice.b@email.com',true),
  (5,'Charlie','Wilson','charlie.w@email.com',true), (6,'Diana','Prince','diana.p@email.com',true),
  (7,'Eve','Martinez','eve.m@email.com',true), (8,'Frank','Castle','frank.c@email.com',false),
  (9,'Grace','Lee','grace.l@email.com',true), (10,'Hank','Green','hank.g@email.com',true)
ON CONFLICT (id) DO NOTHING;

-- Inventory: 2-3 copies per film
INSERT INTO inventory (id, film_id, store_id) VALUES
  (1,1,1),(2,1,1),(3,1,1),(4,2,1),(5,2,1),(6,3,1),(7,3,1),(8,3,1),(9,4,1),(10,4,1),
  (11,5,1),(12,5,1),(13,6,1),(14,6,1),(15,6,1),(16,7,1),(17,7,1),(18,8,1),(19,8,1),
  (20,9,1),(21,9,1),(22,9,1),(23,10,1),(24,10,1),(25,11,1),(26,11,1),(27,12,1),(28,12,1),
  (29,13,1),(30,13,1),(31,13,1),(32,14,1),(33,14,1),(34,15,1),(35,15,1),(36,16,1),(37,16,1),
  (38,17,1),(39,17,1),(40,17,1),(41,18,1),(42,18,1),(43,19,1),(44,19,1),(45,20,1),(46,20,1),(47,20,1)
ON CONFLICT (id) DO NOTHING;
