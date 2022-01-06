// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// Status of the supply delivery.
  /// </summary>
  public static class SupplydeliveryStatusCodes
  {
    /// <summary>
    /// Delivery was not completed.
    /// </summary>
    public static readonly Coding Abandoned = new Coding
    {
      Code = "abandoned",
      Display = "Abandoned",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };
    /// <summary>
    /// Supply has been delivered ("completed").
    /// </summary>
    public static readonly Coding Delivered = new Coding
    {
      Code = "completed",
      Display = "Delivered",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };
    /// <summary>
    /// This electronic record should never have existed, though it is possible that real-world decisions were based on it. (If real-world activity has occurred, the status should be "abandoned" rather than "entered-in-error".).
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered In Error",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };
    /// <summary>
    /// Supply has been requested, but not delivered.
    /// </summary>
    public static readonly Coding InProgress = new Coding
    {
      Code = "in-progress",
      Display = "In Progress",
      System = "http://hl7.org/fhir/supplydelivery-status"
    };

    /// <summary>
    /// Literal for code: Abandoned
    /// </summary>
    public const string LiteralAbandoned = "abandoned";

    /// <summary>
    /// Literal for code: Delivered
    /// </summary>
    public const string LiteralDelivered = "completed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: InProgress
    /// </summary>
    public const string LiteralInProgress = "in-progress";
  };
}
