CREATE ROLE querease LOGIN
  ENCRYPTED PASSWORD 'md501334677d5cd78f1317696b591ce93e2'
   VALID UNTIL 'infinity';

CREATE DATABASE querease
  WITH TEMPLATE template0
       OWNER      = querease
       ENCODING   = 'UTF8'
       LC_COLLATE = 'lv_LV.UTF-8'
       LC_CTYPE   = 'lv_LV.UTF-8'
       CONNECTION LIMIT = -1;
