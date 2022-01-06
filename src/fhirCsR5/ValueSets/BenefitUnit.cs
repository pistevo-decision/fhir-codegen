// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// This value set includes a smattering of Unit type codes.
  /// </summary>
  public static class BenefitUnitCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Family = new Coding
    {
      Code = "family",
      Display = "Family",
      System = "http://terminology.hl7.org/CodeSystem/benefit-unit"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Individual = new Coding
    {
      Code = "individual",
      Display = "Individual",
      System = "http://terminology.hl7.org/CodeSystem/benefit-unit"
    };

    /// <summary>
    /// Literal for code: Family
    /// </summary>
    public const string LiteralFamily = "family";

    /// <summary>
    /// Literal for code: Individual
    /// </summary>
    public const string LiteralIndividual = "individual";
  };
}
