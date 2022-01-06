// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes sample Contract Term Type codes.
  /// </summary>
  public static class ContractTermTypeCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Statutory = new Coding
    {
      Code = "statutory",
      Display = "Statutory",
      System = "http://terminology.hl7.org/CodeSystem/contracttermtypecodes"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding SubjectTo = new Coding
    {
      Code = "subject-to",
      Display = "Subject To",
      System = "http://terminology.hl7.org/CodeSystem/contracttermtypecodes"
    };

    /// <summary>
    /// Literal for code: Statutory
    /// </summary>
    public const string LiteralStatutory = "statutory";

    /// <summary>
    /// Literal for code: SubjectTo
    /// </summary>
    public const string LiteralSubjectTo = "subject-to";
  };
}
