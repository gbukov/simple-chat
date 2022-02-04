CREATE SCHEMA `db_p_n` ;

CREATE TABLE `db_p_n`.`messages` (
  `msgId` INT NOT NULL AUTO_INCREMENT,
  `uName` VARCHAR(45) NOT NULL,
  `room` VARCHAR(45) NOT NULL,
  `message` VARCHAR(1000) NOT NULL,
  PRIMARY KEY (`msgId`));

ALTER TABLE `db_p_n`.`messages`
ADD COLUMN `date` DATETIME NOT NULL AFTER `message`;

ALTER TABLE `db_p_n`.`messages`
CHANGE COLUMN `date` `date` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP;

INSERT INTO `db_p_n`.`messages`
(`uName`,`room`,`message`)
VALUES
("@SYSTEM","General","init");
