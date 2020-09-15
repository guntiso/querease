import org.scalatest.flatspec.{AnyFlatSpec => FlatSpec}
import org.scalatest.matchers.should.Matchers
import org.mojoz.querease._

class CursorsTests extends FlatSpec with Matchers {

  val querease = new Querease with ScalaDtoQuereaseIo

  it should "match 1 cursor data" in {
    val data = Map("name" -> "Sales", "loc" -> "Riga")
    querease.cursorData(data, "dept", Map()) should be {
      Map("dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
    }
  }

  it should "match 2 cursor data" in {
    val data = Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
      List(Map("name" -> "scott", "job" -> "analyst"), Map("name" -> "ziga", "job" -> "developer")))
    querease.cursorData(data, "dept", Map()) should be {
      Map("dept_emps" -> Vector(Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
        Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
        "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
    }

    val data1 = Map("emps" -> List(Map("name" -> "scott", "job" -> "analyst"), Map("name" -> "ziga", "job" -> "developer")))
    querease.cursorData(data1, "dept", Map()) should be {
      Map("dept_emps" -> Vector(Map("name" -> "scott", "job" -> "analyst"), Map("name" -> "ziga", "job" -> "developer")), "dept" -> Vector())
    }
  }

  it should "match 3 cursor data" in {
    val data = Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
      List(Map("name" -> "scott", "work" ->
        List(Map("name" -> "meet", "hours" -> 4), Map("name" -> "review", "hours" -> 2)), "job" -> "analyst"),
        Map("name" -> "ziga", "job" -> "developer", "work" -> List(Map("name" -> "meet", "hours" -> 4)))))
    querease.cursorData(data, "dept", Map()) should be {
      Map("dept_emps_work" ->
        Vector(
          Map("name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> 4, "emps_name" -> "scott", "loc" -> "Riga"),
          Map("name" -> "Sales", "work_name" -> "review", "job" -> "analyst", "hours" -> 2, "emps_name" -> "scott", "loc" -> "Riga"),
          Map("name" -> "Sales", "work_name" -> "meet", "job" -> "developer", "hours" -> 4, "emps_name" -> "ziga", "loc" -> "Riga")),
        "dept_emps" ->
          Vector(
            Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
            Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
        "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
    }

    val data1 = Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
      List(Map("name" -> "scott", "work" ->
        List(Map("name" -> "meet", "hours" -> 4), Map("name" -> "review", "hours" -> 2)), "job" -> "analyst"),
        Map("name" -> "ziga", "job" -> "developer", "work" -> Nil)))
    querease.cursorData(data1, "dept", Map()) should be {
      Map("dept_emps_work" ->
        Vector(
          Map("name" -> "Sales", "work_name" -> "meet", "job" -> "analyst", "hours" -> 4, "emps_name" -> "scott", "loc" -> "Riga"),
          Map("name" -> "Sales", "work_name" -> "review", "job" -> "analyst", "hours" -> 2, "emps_name" -> "scott", "loc" -> "Riga")),
        "dept_emps" ->
          Vector(
            Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
            Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
        "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
    }
  }

  val data2 = Map("name" -> "Sales", "loc" -> "Riga", "emps" ->
    List(Map("name" -> "scott", "work" -> Nil, "job" -> "analyst"),
      Map("name" -> "ziga", "job" -> "developer", "work" -> List(Map("name" -> "meet", "hours" -> 4)))))
  querease.cursorData(data2, "dept", Map()) should be {
    Map("dept_emps_work" ->
      Vector(
        Map("name" -> "Sales", "work_name" -> "meet", "job" -> "developer", "hours" -> 4, "emps_name" -> "ziga", "loc" -> "Riga")),
      "dept_emps" ->
        Vector(
          Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "scott", "job" -> "analyst"),
          Map("name" -> "Sales", "loc" -> "Riga", "emps_name" -> "ziga", "job" -> "developer")),
      "dept" -> Vector(Map("name" -> "Sales", "loc" -> "Riga")))
  }
}
