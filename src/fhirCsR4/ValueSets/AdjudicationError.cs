// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set includes a smattering of adjudication codes.
  /// </summary>
  public static class AdjudicationErrorCodes
  {
    /// <summary>
    /// Missing Identifier
    /// </summary>
    public static readonly Coding MissingIdentifier = new Coding
    {
      Code = "a001",
      Display = "Missing Identifier",
      System = "http://terminology.hl7.org/CodeSystem/adjudication-error"
    };
    /// <summary>
    /// Missing Creation Date
    /// </summary>
    public static readonly Coding MissingCreationDate = new Coding
    {
      Code = "a002",
      Display = "Missing Creation Date",
      System = "http://terminology.hl7.org/CodeSystem/adjudication-error"
    };

    /// <summary>
    /// Literal for code: MissingIdentifier
    /// </summary>
    public const string LiteralMissingIdentifier = "a001";

    /// <summary>
    /// Literal for code: MissingCreationDate
    /// </summary>
    public const string LiteralMissingCreationDate = "a002";
  };
}
