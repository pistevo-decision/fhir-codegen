// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// The verification status to support or decline the clinical status of the condition or diagnosis.
  /// </summary>
  public static class ConditionVerStatusCodes
  {
    /// <summary>
    /// There is sufficient diagnostic and/or clinical evidence to treat this as a confirmed condition.
    /// </summary>
    public static readonly Coding Confirmed = new Coding
    {
      Code = "confirmed",
      Display = "Confirmed",
      System = "http://terminology.hl7.org/CodeSystem/condition-ver-status"
    };
    /// <summary>
    /// One of a set of potential (and typically mutually exclusive) diagnoses asserted to further guide the diagnostic process and preliminary treatment.
    /// </summary>
    public static readonly Coding Differential = new Coding
    {
      Code = "differential",
      Display = "Differential",
      System = "http://terminology.hl7.org/CodeSystem/condition-ver-status"
    };
    /// <summary>
    /// The statement was entered in error and is not valid.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://terminology.hl7.org/CodeSystem/condition-ver-status"
    };
    /// <summary>
    /// This is a tentative diagnosis - still a candidate that is under consideration.
    /// </summary>
    public static readonly Coding Provisional = new Coding
    {
      Code = "provisional",
      Display = "Provisional",
      System = "http://terminology.hl7.org/CodeSystem/condition-ver-status"
    };
    /// <summary>
    /// This condition has been ruled out by diagnostic and clinical evidence.
    /// </summary>
    public static readonly Coding Refuted = new Coding
    {
      Code = "refuted",
      Display = "Refuted",
      System = "http://terminology.hl7.org/CodeSystem/condition-ver-status"
    };
    /// <summary>
    /// There is not sufficient diagnostic and/or clinical evidence to treat this as a confirmed condition.
    /// </summary>
    public static readonly Coding Unconfirmed = new Coding
    {
      Code = "unconfirmed",
      Display = "Unconfirmed",
      System = "http://terminology.hl7.org/CodeSystem/condition-ver-status"
    };

    /// <summary>
    /// Literal for code: Confirmed
    /// </summary>
    public const string LiteralConfirmed = "confirmed";

    /// <summary>
    /// Literal for code: Differential
    /// </summary>
    public const string LiteralDifferential = "differential";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: Provisional
    /// </summary>
    public const string LiteralProvisional = "provisional";

    /// <summary>
    /// Literal for code: Refuted
    /// </summary>
    public const string LiteralRefuted = "refuted";

    /// <summary>
    /// Literal for code: Unconfirmed
    /// </summary>
    public const string LiteralUnconfirmed = "unconfirmed";
  };
}
