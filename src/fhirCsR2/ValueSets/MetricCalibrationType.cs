// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Describes the type of a metric calibration.
  /// </summary>
  public static class MetricCalibrationTypeCodes
  {
    /// <summary>
    /// TODO
    /// </summary>
    public static readonly Coding Gain = new Coding
    {
      Code = "gain",
      Display = "Gain",
      System = "http://hl7.org/fhir/metric-calibration-type"
    };
    /// <summary>
    /// TODO
    /// </summary>
    public static readonly Coding Offset = new Coding
    {
      Code = "offset",
      Display = "Offset",
      System = "http://hl7.org/fhir/metric-calibration-type"
    };
    /// <summary>
    /// TODO
    /// </summary>
    public static readonly Coding TwoPoint = new Coding
    {
      Code = "two-point",
      Display = "Two Point",
      System = "http://hl7.org/fhir/metric-calibration-type"
    };
    /// <summary>
    /// TODO
    /// </summary>
    public static readonly Coding Unspecified = new Coding
    {
      Code = "unspecified",
      Display = "Unspecified",
      System = "http://hl7.org/fhir/metric-calibration-type"
    };

    /// <summary>
    /// Literal for code: Gain
    /// </summary>
    public const string LiteralGain = "gain";

    /// <summary>
    /// Literal for code: Offset
    /// </summary>
    public const string LiteralOffset = "offset";

    /// <summary>
    /// Literal for code: TwoPoint
    /// </summary>
    public const string LiteralTwoPoint = "two-point";

    /// <summary>
    /// Literal for code: Unspecified
    /// </summary>
    public const string LiteralUnspecified = "unspecified";
  };
}
