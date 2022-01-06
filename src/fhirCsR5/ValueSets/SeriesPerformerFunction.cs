// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// Performer function of an agent in an imaging study series
  /// </summary>
  public static class SeriesPerformerFunctionCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Consultant = new Coding
    {
      Code = "CON",
      Display = "consultant",
      System = "http://terminology.hl7.org/CodeSystem/v3-ParticipationType"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Performer = new Coding
    {
      Code = "PRF",
      Display = "performer",
      System = "http://terminology.hl7.org/CodeSystem/v3-ParticipationType"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Referrer = new Coding
    {
      Code = "REF",
      Display = "referrer",
      System = "http://terminology.hl7.org/CodeSystem/v3-ParticipationType"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding SecondaryPerformer = new Coding
    {
      Code = "SPRF",
      Display = "secondary performer",
      System = "http://terminology.hl7.org/CodeSystem/v3-ParticipationType"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding Verifier = new Coding
    {
      Code = "VRF",
      Display = "verifier",
      System = "http://terminology.hl7.org/CodeSystem/v3-ParticipationType"
    };

    /// <summary>
    /// Literal for code: Consultant
    /// </summary>
    public const string LiteralConsultant = "CON";

    /// <summary>
    /// Literal for code: Performer
    /// </summary>
    public const string LiteralPerformer = "PRF";

    /// <summary>
    /// Literal for code: Referrer
    /// </summary>
    public const string LiteralReferrer = "REF";

    /// <summary>
    /// Literal for code: SecondaryPerformer
    /// </summary>
    public const string LiteralSecondaryPerformer = "SPRF";

    /// <summary>
    /// Literal for code: Verifier
    /// </summary>
    public const string LiteralVerifier = "VRF";
  };
}
