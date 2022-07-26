// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes example Procedure Type codes.
  /// </summary>
  public static class ExProcedureTypeCodes
  {
    /// <summary>
    /// Primary procedure
    /// </summary>
    public static readonly Coding PrimaryProcedure = new Coding
    {
      Code = "primary",
      Display = "Primary procedure",
      System = "http://terminology.hl7.org/CodeSystem/ex-procedure-type"
    };
    /// <summary>
    /// Secondary procedure
    /// </summary>
    public static readonly Coding SecondaryProcedure = new Coding
    {
      Code = "secondary",
      Display = "Secondary procedure",
      System = "http://terminology.hl7.org/CodeSystem/ex-procedure-type"
    };

    /// <summary>
    /// Literal for code: PrimaryProcedure
    /// </summary>
    public const string LiteralPrimaryProcedure = "primary";

    /// <summary>
    /// Literal for code: ExProcedureTypePrimaryProcedure
    /// </summary>
    public const string LiteralExProcedureTypePrimaryProcedure = "http://terminology.hl7.org/CodeSystem/ex-procedure-type#primary";

    /// <summary>
    /// Literal for code: SecondaryProcedure
    /// </summary>
    public const string LiteralSecondaryProcedure = "secondary";

    /// <summary>
    /// Literal for code: ExProcedureTypeSecondaryProcedure
    /// </summary>
    public const string LiteralExProcedureTypeSecondaryProcedure = "http://terminology.hl7.org/CodeSystem/ex-procedure-type#secondary";

    /// <summary>
    /// Dictionary for looking up ExProcedureType Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "primary", PrimaryProcedure }, 
      { "http://terminology.hl7.org/CodeSystem/ex-procedure-type#primary", PrimaryProcedure }, 
      { "secondary", SecondaryProcedure }, 
      { "http://terminology.hl7.org/CodeSystem/ex-procedure-type#secondary", SecondaryProcedure }, 
    };
  };
}