// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Is the Participant required to attend the appointment.
  /// </summary>
  public static class ParticipantrequiredCodes
  {
    /// <summary>
    /// The participant is excluded from the appointment, and may not be informed of the appointment taking place. (Appointment is about them, not for them - such as 2 doctors discussing results about a patient's test).
    /// </summary>
    public static readonly Coding InformationOnly = new Coding
    {
      Code = "information-only",
      Display = "Information Only",
      System = "http://hl7.org/fhir/participantrequired"
    };
    /// <summary>
    /// The participant may optionally attend the appointment.
    /// </summary>
    public static readonly Coding Optional = new Coding
    {
      Code = "optional",
      Display = "Optional",
      System = "http://hl7.org/fhir/participantrequired"
    };
    /// <summary>
    /// The participant is required to attend the appointment.
    /// </summary>
    public static readonly Coding Required = new Coding
    {
      Code = "required",
      Display = "Required",
      System = "http://hl7.org/fhir/participantrequired"
    };

    /// <summary>
    /// Literal for code: InformationOnly
    /// </summary>
    public const string LiteralInformationOnly = "information-only";

    /// <summary>
    /// Literal for code: Optional
    /// </summary>
    public const string LiteralOptional = "optional";

    /// <summary>
    /// Literal for code: Required
    /// </summary>
    public const string LiteralRequired = "required";
  };
}
