// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// SHALL applications comply with this constraint?
  /// </summary>
  public static class ConstraintSeverityCodes
  {
    /// <summary>
    /// If the constraint is violated, the resource is not conformant.
    /// </summary>
    public static readonly Coding Error = new Coding
    {
      Code = "error",
      Display = "Error",
      System = "http://hl7.org/fhir/constraint-severity"
    };
    /// <summary>
    /// If the constraint is violated, the resource is conformant, but it is not necessarily following best practice.
    /// </summary>
    public static readonly Coding Warning = new Coding
    {
      Code = "warning",
      Display = "Warning",
      System = "http://hl7.org/fhir/constraint-severity"
    };

    /// <summary>
    /// Literal for code: Error
    /// </summary>
    public const string LiteralError = "error";

    /// <summary>
    /// Literal for code: Warning
    /// </summary>
    public const string LiteralWarning = "warning";
  };
}
