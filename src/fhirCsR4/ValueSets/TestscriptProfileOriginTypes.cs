// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set defines a set of codes that are used to indicate the profile type of a test system when acting as the origin within a TestScript.
  /// </summary>
  public static class TestscriptProfileOriginTypesCodes
  {
    /// <summary>
    /// General FHIR client used to initiate operations against a FHIR server.
    /// </summary>
    public static readonly Coding FHIRClient = new Coding
    {
      Code = "FHIR-Client",
      Display = "FHIR Client",
      System = "http://terminology.hl7.org/CodeSystem/testscript-profile-origin-types"
    };
    /// <summary>
    /// A FHIR client acting as a Structured Data Capture Form Filler.
    /// </summary>
    public static readonly Coding FHIRSDCFormFiller = new Coding
    {
      Code = "FHIR-SDC-FormFiller",
      Display = "FHIR SDC FormFiller",
      System = "http://terminology.hl7.org/CodeSystem/testscript-profile-origin-types"
    };

    /// <summary>
    /// Literal for code: FHIRClient
    /// </summary>
    public const string LiteralFHIRClient = "FHIR-Client";

    /// <summary>
    /// Literal for code: FHIRSDCFormFiller
    /// </summary>
    public const string LiteralFHIRSDCFormFiller = "FHIR-SDC-FormFiller";
  };
}
