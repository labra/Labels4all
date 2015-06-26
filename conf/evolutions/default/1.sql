# Database schema

# --- !Ups

CREATE TABLE IRIs (
	id BIGSERIAL PRIMARY KEY,
	name VARCHAR(3000) UNIQUE NOT NULL
);

CREATE TABLE Languages (
	id BIGSERIAL PRIMARY KEY,
	code VARCHAR(4) UNIQUE NOT NULL,
	name VARCHAR(100) NOT NULL
);

CREATE TABLE LabelTranslations (
	id BIGSERIAL PRIMARY KEY,
	id_iri BIGINT NOT NULL REFERENCES IRIs(id),
	id_language BIGINT NOT NULL REFERENCES Languages(id),
	content VARCHAR(1000) NOT NULL,
	machine_translated BOOLEAN NOT NULL,
	votes INTEGER NOT NULL
);

ALTER TABLE LabelTranslations
	ADD CONSTRAINT uq_iri_lang_content UNIQUE(id_iri, id_language, content);

INSERT INTO Languages (code, name) VALUES ('aa', 'Afar'), ('ab', 'Abkhazian'), ('ae', 'Avestan'), ('af', 'Afrikaans'), ('ak', 'Akan'), ('am', 'Amharic'), ('an', 'Aragonese'), ('ar', 'Arabic'), ('as', 'Assamese'), ('av', 'Avaric'), ('ay', 'Aymara'), ('az', 'Azerbaijani'), ('ba', 'Bashkir'), ('be', 'Belarusian'), ('bg', 'Bulgarian'), ('bh', 'Bihari languages'), ('bi', 'Bislama'), ('bm', 'Bambara'), ('bn', 'Bengali'), ('bo', 'Tibetan'), ('br', 'Breton'), ('bs', 'Bosnian'), ('ca', 'Catalan'), ('ce', 'Chechen'), ('ch', 'Chamorro'), ('co', 'Corsican'), ('cr', 'Cree'), ('cs', 'Czech'), ('cu', 'Church Slavic'), ('cv', 'Chuvash'), ('cy', 'Welsh'), ('da', 'Danish'), ('de', 'German'), ('dv', 'Dhivehi'), ('dz', 'Dzongkha'), ('ee', 'Ewe'), ('el', 'Modern Greek (1453-)'), ('en', 'English'), ('eo', 'Esperanto'), ('es', 'Spanish'), ('et', 'Estonian'), ('eu', 'Basque'), ('fa', 'Persian'), ('ff', 'Fulah'), ('fi', 'Finnish'), ('fj', 'Fijian'), ('fo', 'Faroese'), ('fr', 'French'), ('fy', 'Western Frisian'), ('ga', 'Irish'), ('gd', 'Scottish Gaelic'), ('gl', 'Galician'), ('gn', 'Guarani'), ('gu', 'Gujarati'), ('gv', 'Manx'), ('ha', 'Hausa'), ('he', 'Hebrew'), ('hi', 'Hindi'), ('ho', 'Hiri Motu'), ('hr', 'Croatian'), ('ht', 'Haitian'), ('hu', 'Hungarian'), ('hy', 'Armenian'), ('hz', 'Herero'), ('ia', 'Interlingua (International Auxiliary Language'), ('id', 'Indonesian'), ('ie', 'Interlingue'), ('ig', 'Igbo'), ('ii', 'Sichuan Yi'), ('ik', 'Inupiaq'), ('in', 'Indonesian'), ('io', 'Ido'), ('is', 'Icelandic'), ('it', 'Italian'), ('iu', 'Inuktitut'), ('iw', 'Hebrew'), ('ja', 'Japanese'), ('ji', 'Yiddish'), ('jv', 'Javanese'), ('jw', 'Javanese'), ('ka', 'Georgian'), ('kg', 'Kongo'), ('ki', 'Kikuyu'), ('kj', 'Kuanyama'), ('kk', 'Kazakh'), ('kl', 'Kalaallisut'), ('km', 'Central Khmer'), ('kn', 'Kannada'), ('ko', 'Korean'), ('kr', 'Kanuri'), ('ks', 'Kashmiri'), ('ku', 'Kurdish'), ('kv', 'Komi'), ('kw', 'Cornish'), ('ky', 'Kirghiz'), ('la', 'Latin'), ('lb', 'Luxembourgish'), ('lg', 'Ganda'), ('li', 'Limburgan'), ('ln', 'Lingala'), ('lo', 'Lao'), ('lt', 'Lithuanian'), ('lu', 'Luba-Katanga'), ('lv', 'Latvian'), ('mg', 'Malagasy'), ('mh', 'Marshallese'), ('mi', 'Maori'), ('mk', 'Macedonian'), ('ml', 'Malayalam'), ('mn', 'Mongolian'), ('mo', 'Moldavian'), ('mr', 'Marathi'), ('ms', 'Malay (macrolanguage)'), ('mt', 'Maltese'), ('my', 'Burmese'), ('na', 'Nauru'), ('nb', 'Norwegian Bokmål'), ('nd', 'North Ndebele'), ('ne', 'Nepali (macrolanguage)'), ('ng', 'Ndonga'), ('nl', 'Dutch'), ('nn', 'Norwegian Nynorsk'), ('no', 'Norwegian'), ('nr', 'South Ndebele'), ('nv', 'Navajo'), ('ny', 'Nyanja'), ('oc', 'Occitan (post 1500)'), ('oj', 'Ojibwa'), ('om', 'Oromo'), ('or', 'Oriya (macrolanguage)'), ('os', 'Ossetian'), ('pa', 'Panjabi'), ('pi', 'Pali'), ('pl', 'Polish'), ('ps', 'Pushto'), ('pt', 'Portuguese'), ('qu', 'Quechua'), ('rm', 'Romansh'), ('rn', 'Rundi'), ('ro', 'Romanian'), ('ru', 'Russian'), ('rw', 'Kinyarwanda'), ('sa', 'Sanskrit'), ('sc', 'Sardinian'), ('sd', 'Sindhi'), ('se', 'Northern Sami'), ('sg', 'Sango'), ('sh', 'Serbo-Croatian'), ('si', 'Sinhala'), ('sk', 'Slovak'), ('sl', 'Slovenian'), ('sm', 'Samoan'), ('sn', 'Shona'), ('so', 'Somali'), ('sq', 'Albanian'), ('sr', 'Serbian'), ('ss', 'Swati'), ('st', 'Southern Sotho'), ('su', 'Sundanese'), ('sv', 'Swedish'), ('sw', 'Swahili (macrolanguage)'), ('ta', 'Tamil'), ('te', 'Telugu'), ('tg', 'Tajik'), ('th', 'Thai'), ('ti', 'Tigrinya'), ('tk', 'Turkmen'), ('tl', 'Tagalog'), ('tn', 'Tswana'), ('to', 'Tonga (Tonga Islands)'), ('tr', 'Turkish'), ('ts', 'Tsonga'), ('tt', 'Tatar'), ('tw', 'Twi'), ('ty', 'Tahitian'), ('ug', 'Uighur'), ('uk', 'Ukrainian'), ('ur', 'Urdu'), ('uz', 'Uzbek'), ('ve', 'Venda'), ('vi', 'Vietnamese'), ('vo', 'Volapük'), ('wa', 'Walloon'), ('wo', 'Wolof'), ('xh', 'Xhosa'), ('yi', 'Yiddish'), ('yo', 'Yoruba'), ('za', 'Zhuang'), ('zh', 'Chinese'), ('zu', 'Zulu')

# --- !Downs

DROP TABLE IRIs, Languages, LabelTranslations CASCADE;
