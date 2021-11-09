CREATE ROLE querease
  LOGIN PASSWORD 'querease'
   VALID UNTIL 'infinity';

CREATE DATABASE querease
  WITH TEMPLATE template0
       OWNER      = querease
       ENCODING   = 'UTF8'
       LC_COLLATE = 'lv_LV.UTF-8'
       LC_CTYPE   = 'lv_LV.UTF-8'
       CONNECTION LIMIT = -1;
