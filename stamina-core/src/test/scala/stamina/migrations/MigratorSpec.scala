package stamina.migrations

import stamina._

class MigratorSpec extends StaminaSpec {

  val mV3: Migrator[String, V3] =
    from[String, V1]
      .to[V2](_ + "V2")
      .to[V3](_ + "V3")

  val mV3WithBackwardMigration: Migrator[String, V3] =
    from[String, V1]
      .to[V2](_ + "V2")
      .to[V3](_ + "V3")
      .andBackFrom[V4](_.replace("V4", ""))

  val mV1WithBackwardMigration: Migrator[String, V1] =
    from[String, V1]
      .andBackFrom[V2](_.replace("V2", ""))

  "Migrator with backward migration" should {
    "be able to migrate" when {
      "migration is from V1" in {
        mV3WithBackwardMigration.canMigrate(1) shouldBe true
      }

      "migration is from V2" in {
        mV3WithBackwardMigration.canMigrate(2) shouldBe true
      }

      "migration is from V3 (identity)" in {
        mV3WithBackwardMigration.canMigrate(3) shouldBe true
      }

      "migration is from V4 (backward migration)" in {
        mV3WithBackwardMigration.canMigrate(4) shouldBe true
      }
    }

    "not be able to migrate" when {
      "migration is from V5" in {
        mV3WithBackwardMigration.canMigrate(5) shouldBe false
      }
    }

    "migrate forward" when {
      "migration is from V1" in {
        mV3WithBackwardMigration.migrate("V1", 1) shouldBe "V1V2V3"
      }

      "migration is from V2" in {
        mV3WithBackwardMigration.migrate("V1V2", 2) shouldBe "V1V2V3"
      }

      "migration is from V3" in {
        mV3WithBackwardMigration.migrate("V1V2V3", 3) shouldBe "V1V2V3"
      }
    }

    "migrate backward" when {
      "migration is from V4" in {
        mV3WithBackwardMigration.migrate("V1V2V3V4", 4) shouldBe "V1V2V3"
      }
    }
  }

  "Migrator with ignored backward migration" should {
    "be able to migrate" when {
      "migration is from V1" in {
        mV3.canMigrate(1) shouldBe true
      }

      "migration is from V2" in {
        mV3.canMigrate(2) shouldBe true
      }

      "migration is from V3 (identity)" in {
        mV3.canMigrate(3) shouldBe true
      }
    }

    "not be able to migrate" when {
      "migration is from V4" in {
        mV3.canMigrate(4) shouldBe false
      }
    }

    "migrate" when {
      "migration is from V1" in {
        mV3.migrate("V1", 1) shouldBe "V1V2V3"
      }

      "migration is from V2" in {
        mV3.migrate("V1V2", 2) shouldBe "V1V2V3"
      }

      "migration is from V3" in {
        mV3.migrate("V1V2V3", 3) shouldBe "V1V2V3"
      }
    }
  }

  "Migrator V1 with backward migration" should {
    "be able to migrate" when {
      "migration is from V2" in {
        mV1WithBackwardMigration.canMigrate(2) shouldBe true
      }
    }

    "migrate" when {
      "migration is from V2" in {
        mV1WithBackwardMigration.migrate("V1V2", 2) shouldBe "V1"
      }
    }
  }
}
