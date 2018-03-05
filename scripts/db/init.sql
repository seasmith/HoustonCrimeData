-- Database: HoustonCrimeData

-- DROP DATABASE "HoustonCrimeData";

CREATE DATABASE "HoustonCrimeData"
    WITH 
    OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'English_United States.1252'
    LC_CTYPE = 'English_United States.1252'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1;

COMMENT ON DATABASE "HoustonCrimeData"
    IS 'For the ''R/HoustonCrimeData'' project';