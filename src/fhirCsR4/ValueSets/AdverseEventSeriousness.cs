// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// Overall seriousness of this event for the patient.
  /// </summary>
  public static class AdverseEventSeriousnessCodes
  {
    /// <summary>
    /// Non-serious.
    /// </summary>
    public static readonly Coding NonSerious = new Coding
    {
      Code = "Non-serious",
      Display = "Non-serious",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Serious.
    /// </summary>
    public static readonly Coding Serious = new Coding
    {
      Code = "Serious",
      Display = "Serious",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Is a congenital anomaly/birth defect.
    /// </summary>
    public static readonly Coding IsACongenitalAnomalyBirthDefect = new Coding
    {
      Code = "SeriousIsBirthDefect",
      Display = "Is a congenital anomaly/birth defect",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Is Life-threatening.
    /// </summary>
    public static readonly Coding IsLifeThreatening = new Coding
    {
      Code = "SeriousIsLifeThreatening",
      Display = "Is Life-threatening",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Requires intervention to prevent permanent impairment or damage (i.e., an important medical event that requires medical judgement).
    /// </summary>
    public static readonly Coding RequiresInterventionToPreventPermanentImpairment = new Coding
    {
      Code = "SeriousRequiresPreventImpairment",
      Display = "Requires intervention to prevent permanent impairment",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Results in death.
    /// </summary>
    public static readonly Coding ResultsInDeath = new Coding
    {
      Code = "SeriousResultsInDeath",
      Display = "Results in death",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Results in persistent or significant disability/incapacity.
    /// </summary>
    public static readonly Coding ResultsInPersistentOrSignificantDisabilityIncapacity = new Coding
    {
      Code = "SeriousResultsInDisability",
      Display = "Results in persistent or significant disability/incapacity",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };
    /// <summary>
    /// Requires inpatient hospitalization or causes prolongation of existing hospitalization.
    /// </summary>
    public static readonly Coding RequiresOrProlongsInpatientHospitalization = new Coding
    {
      Code = "SeriousResultsInHospitalization",
      Display = "Requires or prolongs inpatient hospitalization",
      System = "http://terminology.hl7.org/CodeSystem/adverse-event-seriousness"
    };

    /// <summary>
    /// Literal for code: NonSerious
    /// </summary>
    public const string LiteralNonSerious = "Non-serious";

    /// <summary>
    /// Literal for code: Serious
    /// </summary>
    public const string LiteralSerious = "Serious";

    /// <summary>
    /// Literal for code: IsACongenitalAnomalyBirthDefect
    /// </summary>
    public const string LiteralIsACongenitalAnomalyBirthDefect = "SeriousIsBirthDefect";

    /// <summary>
    /// Literal for code: IsLifeThreatening
    /// </summary>
    public const string LiteralIsLifeThreatening = "SeriousIsLifeThreatening";

    /// <summary>
    /// Literal for code: RequiresInterventionToPreventPermanentImpairment
    /// </summary>
    public const string LiteralRequiresInterventionToPreventPermanentImpairment = "SeriousRequiresPreventImpairment";

    /// <summary>
    /// Literal for code: ResultsInDeath
    /// </summary>
    public const string LiteralResultsInDeath = "SeriousResultsInDeath";

    /// <summary>
    /// Literal for code: ResultsInPersistentOrSignificantDisabilityIncapacity
    /// </summary>
    public const string LiteralResultsInPersistentOrSignificantDisabilityIncapacity = "SeriousResultsInDisability";

    /// <summary>
    /// Literal for code: RequiresOrProlongsInpatientHospitalization
    /// </summary>
    public const string LiteralRequiresOrProlongsInpatientHospitalization = "SeriousResultsInHospitalization";
  };
}
