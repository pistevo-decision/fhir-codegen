// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Codes representing the current status of the device - on, off, suspended, etc.
  /// </summary>
  public static class OperationalStatusCodes
  {
    /// <summary>
    /// The device was entered in error.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered In Error",
      System = "http://hl7.org/fhir/operational-status"
    };
    /// <summary>
    /// The device hardware is disconnected.
    /// </summary>
    public static readonly Coding HardwareDisconnectd = new Coding
    {
      Code = "hw-discon",
      Display = "Hardware Disconnectd",
      System = "http://hl7.org/fhir/operational-status"
    };
    /// <summary>
    /// The device is not ready.
    /// </summary>
    public static readonly Coding NotReady = new Coding
    {
      Code = "not-ready",
      Display = "Not Ready",
      System = "http://hl7.org/fhir/operational-status"
    };
    /// <summary>
    /// The device is off.
    /// </summary>
    public static readonly Coding Off = new Coding
    {
      Code = "off",
      Display = "Off",
      System = "http://hl7.org/fhir/operational-status"
    };
    /// <summary>
    /// The device is fully operational.
    /// </summary>
    public static readonly Coding On = new Coding
    {
      Code = "on",
      Display = "On",
      System = "http://hl7.org/fhir/operational-status"
    };
    /// <summary>
    /// The device is ready but not actively operating.
    /// </summary>
    public static readonly Coding Standby = new Coding
    {
      Code = "standby",
      Display = "Standby",
      System = "http://hl7.org/fhir/operational-status"
    };
    /// <summary>
    /// The device transducer is diconnected.
    /// </summary>
    public static readonly Coding TransducerDiconnected = new Coding
    {
      Code = "transduc-discon",
      Display = "Transducer Diconnected",
      System = "http://hl7.org/fhir/operational-status"
    };

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: HardwareDisconnectd
    /// </summary>
    public const string LiteralHardwareDisconnectd = "hw-discon";

    /// <summary>
    /// Literal for code: NotReady
    /// </summary>
    public const string LiteralNotReady = "not-ready";

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

    /// <summary>
    /// Literal for code: TransducerDiconnected
    /// </summary>
    public const string LiteralTransducerDiconnected = "transduc-discon";
  };
}
