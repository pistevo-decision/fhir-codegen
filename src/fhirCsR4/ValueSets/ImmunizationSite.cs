// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the body site where the vaccination occurred. This value set is provided as a suggestive example.
  /// </summary>
  public static class ImmunizationSiteCodes
  {
    /// <summary>
    /// left arm
    /// </summary>
    public static readonly Coding LeftArm = new Coding
    {
      Code = "LA",
      Display = "left arm",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActSite"
    };
    /// <summary>
    /// right arm
    /// </summary>
    public static readonly Coding RightArm = new Coding
    {
      Code = "RA",
      Display = "right arm",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActSite"
    };

    /// <summary>
    /// Literal for code: LeftArm
    /// </summary>
    public const string LiteralLeftArm = "LA";

    /// <summary>
    /// Literal for code: V3ActSiteLeftArm
    /// </summary>
    public const string LiteralV3ActSiteLeftArm = "http://terminology.hl7.org/CodeSystem/v3-ActSite#LA";

    /// <summary>
    /// Literal for code: RightArm
    /// </summary>
    public const string LiteralRightArm = "RA";

    /// <summary>
    /// Literal for code: V3ActSiteRightArm
    /// </summary>
    public const string LiteralV3ActSiteRightArm = "http://terminology.hl7.org/CodeSystem/v3-ActSite#RA";

    /// <summary>
    /// Dictionary for looking up ImmunizationSite Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "LA", LeftArm }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActSite#LA", LeftArm }, 
      { "RA", RightArm }, 
      { "http://terminology.hl7.org/CodeSystem/v3-ActSite#RA", RightArm }, 
    };
  };
}