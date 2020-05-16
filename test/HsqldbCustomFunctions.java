package test;

public class HsqldbCustomFunctions {
  public static java.lang.Integer array_length(java.sql.Array sql_array) throws java.sql.SQLException {
    Object[] array = (Object[]) sql_array.getArray();
    if (array == null)
      return null;
    else
      return array.length;
  }
}
