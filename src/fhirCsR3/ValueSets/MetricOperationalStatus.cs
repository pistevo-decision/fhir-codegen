// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Describes the operational status of the DeviceMetric.
  /// </summary>
  public static class MetricOperationalStatusCodes
  {
    /// <summary>
    /// The DeviceMetric was entered in error.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered In Error",
      System = "http://hl7.org/fhir/metric-operational-status"
    };
    /// <summary>
    /// The DeviceMetric is not operating.
    /// </summary>
    public static readonly Coding Off = new Coding
    {
      Code = "off",
      Display = "Off",
      System = "http://hl7.org/fhir/metric-operational-status"
    };
    /// <summary>
    /// The DeviceMetric is operating and will generate DeviceObservations.
    /// </summary>
    public static readonly Coding On = new Coding
    {
      Code = "on",
      Display = "On",
      System = "http://hl7.org/fhir/metric-operational-status"
    };
    /// <summary>
    /// The DeviceMetric is operating, but will not generate any DeviceObservations.
    /// </summary>
    public static readonly Coding Standby = new Coding
    {
      Code = "standby",
      Display = "Standby",
      System = "http://hl7.org/fhir/metric-operational-status"
    };

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: Off
    /// </summary>
    public const string LiteralOff = "off";

    /// <summary>
    /// Literal for code: On
    /// </summary>
    public const string LiteralOn = "on";

    /// <summary>
    /// Literal for code: Standby
    /// </summary>
    public const string LiteralStandby = "standby";
  };
}
