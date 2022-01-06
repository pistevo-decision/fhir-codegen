// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Availability of the resource
  /// </summary>
  public static class InstanceAvailabilityCodes
  {
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding NEARLINE = new Coding
    {
      Code = "NEARLINE",
      System = "http://nema.org/dicom/dicm"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding OFFLINE = new Coding
    {
      Code = "OFFLINE",
      System = "http://nema.org/dicom/dicm"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding ONLINE = new Coding
    {
      Code = "ONLINE",
      System = "http://nema.org/dicom/dicm"
    };
    /// <summary>
    /// 
    /// </summary>
    public static readonly Coding UNAVAILABLE = new Coding
    {
      Code = "UNAVAILABLE",
      System = "http://nema.org/dicom/dicm"
    };

    /// <summary>
    /// Literal for code: NEARLINE
    /// </summary>
    public const string LiteralNEARLINE = "NEARLINE";

    /// <summary>
    /// Literal for code: OFFLINE
    /// </summary>
    public const string LiteralOFFLINE = "OFFLINE";

    /// <summary>
    /// Literal for code: ONLINE
    /// </summary>
    public const string LiteralONLINE = "ONLINE";

    /// <summary>
    /// Literal for code: UNAVAILABLE
    /// </summary>
    public const string LiteralUNAVAILABLE = "UNAVAILABLE";
  };
}
