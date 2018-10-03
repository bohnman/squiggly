INSERT INTO person (id, first_name, last_name) VALUES (1, 'Tyrion', 'Lannister');
INSERT INTO person (id, first_name, last_name) VALUES (2, 'Ned', 'Stark');

INSERT INTO concert(id, description) VALUES (1, 'Rolling Stones');
INSERT INTO concert(id, description) VALUES (2, 'Chainsmokers');


INSERT INTO concert_person(concert_id, person_id) VALUES (1, 1);
INSERT INTO concert_person(concert_id, person_id) VALUES (1, 2);