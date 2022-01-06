// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set includes sample Related Claim Relationship codes.
  /// </summary>
  public static class RelatedClaimRelationshipCodes
  {
    /// <summary>
    /// A claim for a different suite of services which is related the suite claimed here.
    /// </summary>
    public static readonly Coding AssociatedClaim = new Coding
    {
      Code = "associated",
      Display = "Associated Claim",
      System = "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship"
    };
    /// <summary>
    /// A prior claim instance for the same intended suite of services.
    /// </summary>
    public static readonly Coding PriorClaim = new Coding
    {
      Code = "prior",
      Display = "Prior Claim",
      System = "http://terminology.hl7.org/CodeSystem/ex-relatedclaimrelationship"
    };

    /// <summary>
    /// Literal for code: AssociatedClaim
    /// </summary>
    public const string LiteralAssociatedClaim = "associated";

    /// <summary>
    /// Literal for code: PriorClaim
    /// </summary>
    public const string LiteralPriorClaim = "prior";
  };
}
