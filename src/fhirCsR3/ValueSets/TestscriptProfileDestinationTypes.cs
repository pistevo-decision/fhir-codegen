// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// This value set defines a set of codes that are used to indicate the profile type of a test system when acting as the destination within a TestScript.
  /// </summary>
  public static class TestscriptProfileDestinationTypesCodes
  {
    /// <summary>
    /// A FHIR server acting as a Structured Data Capture Form Manager.
    /// </summary>
    public static readonly Coding FHIRSDCFormManager = new Coding
    {
      Code = "FHIR-SDC-FormManager",
      Display = "FHIR SDC FormManager",
      System = "http://hl7.org/fhir/testscript-profile-destination-types"
    };
    /// <summary>
    /// A FHIR server acting as a Structured Data Capture Form Processor.
    /// </summary>
    public static readonly Coding FHIRSDCFormProcessor = new Coding
    {
      Code = "FHIR-SDC-FormProcessor",
      Display = "FHIR SDC FormProcessor",
      System = "http://hl7.org/fhir/testscript-profile-destination-types"
    };
    /// <summary>
    /// A FHIR server acting as a Structured Data Capture Form Receiver.
    /// </summary>
    public static readonly Coding FHIRSDCFormReceiver = new Coding
    {
      Code = "FHIR-SDC-FormReceiver",
      Display = "FHIR SDC FormReceiver",
      System = "http://hl7.org/fhir/testscript-profile-destination-types"
    };
    /// <summary>
    /// General FHIR server used to respond to operations sent from a FHIR client.
    /// </summary>
    public static readonly Coding FHIRServer = new Coding
    {
      Code = "FHIR-Server",
      Display = "FHIR Server",
      System = "http://hl7.org/fhir/testscript-profile-destination-types"
    };

    /// <summary>
    /// Literal for code: FHIRSDCFormManager
    /// </summary>
    public const string LiteralFHIRSDCFormManager = "FHIR-SDC-FormManager";

    /// <summary>
    /// Literal for code: FHIRSDCFormProcessor
    /// </summary>
    public const string LiteralFHIRSDCFormProcessor = "FHIR-SDC-FormProcessor";

    /// <summary>
    /// Literal for code: FHIRSDCFormReceiver
    /// </summary>
    public const string LiteralFHIRSDCFormReceiver = "FHIR-SDC-FormReceiver";

    /// <summary>
    /// Literal for code: FHIRServer
    /// </summary>
    public const string LiteralFHIRServer = "FHIR-Server";
  };
}
