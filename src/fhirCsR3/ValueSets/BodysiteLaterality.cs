// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Laterality: SNOMED-CT concepts for 'left', 'right', and 'bilateral'
  /// </summary>
  public static class BodysiteLateralityCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding UnilateralLeft = new Coding
    {
      Code = "419161000",
      Display = "Unilateral left",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding UnilateralRight = new Coding
    {
      Code = "419465000",
      Display = "Unilateral right",
      System = "http://snomed.info/sct"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Bilateral = new Coding
    {
      Code = "51440002",
      Display = "Bilateral",
      System = "http://snomed.info/sct"
    };

    /// <summary>
    /// Literal for code: UnilateralLeft
    /// </summary>
    public const string LiteralUnilateralLeft = "419161000";

    /// <summary>
    /// Literal for code: UnilateralRight
    /// </summary>
    public const string LiteralUnilateralRight = "419465000";

    /// <summary>
    /// Literal for code: Bilateral
    /// </summary>
    public const string LiteralBilateral = "51440002";
  };
}
