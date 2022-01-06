// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Whether an operation parameter is an input or an output parameter.
  /// </summary>
  public static class OperationParameterUseCodes
  {
    /// <summary>
    /// This is an input parameter.
    /// </summary>
    public static readonly Coding In = new Coding
    {
      Code = "in",
      Display = "In",
      System = "http://hl7.org/fhir/operation-parameter-use"
    };
    /// <summary>
    /// This is an output parameter.
    /// </summary>
    public static readonly Coding Out = new Coding
    {
      Code = "out",
      Display = "Out",
      System = "http://hl7.org/fhir/operation-parameter-use"
    };

    /// <summary>
    /// Literal for code: In
    /// </summary>
    public const string LiteralIn = "in";

    /// <summary>
    /// Literal for code: Out
    /// </summary>
    public const string LiteralOut = "out";
  };
}
