package test;

public class HsqldbCustomFunctions {
  public static java.lang.Long checked_resolve_l(
      String resolvable, java.sql.Array resolved, String error_message) throws java.sql.SQLException {
    Object[] resolved_a = (Object[]) resolved.getArray();
    if (resolved_a.length > 1 || resolvable != null && (resolved_a.length == 0 || resolved_a[0] == null))
      throw new RuntimeException(error_message);
    else
      return (Long) resolved_a[0];
  }
}
