// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// Lifecycle status of the questionnaire response.
  /// </summary>
  public static class QuestionnaireAnswersStatusCodes
  {
    /// <summary>
    /// This QuestionnaireResponse has been filled out with answers, then marked as complete, yet changes or additions have been made to it afterwards.
    /// </summary>
    public static readonly Coding Amended = new Coding
    {
      Code = "amended",
      Display = "Amended",
      System = "http://hl7.org/fhir/questionnaire-answers-status"
    };
    /// <summary>
    /// This QuestionnaireResponse has been filled out with answers, and the current content is regarded as definitive.
    /// </summary>
    public static readonly Coding Completed = new Coding
    {
      Code = "completed",
      Display = "Completed",
      System = "http://hl7.org/fhir/questionnaire-answers-status"
    };
    /// <summary>
    /// This QuestionnaireResponse has been partially filled out with answers, but changes or additions are still expected to be made to it.
    /// </summary>
    public static readonly Coding InProgress = new Coding
    {
      Code = "in-progress",
      Display = "In Progress",
      System = "http://hl7.org/fhir/questionnaire-answers-status"
    };

    /// <summary>
    /// Literal for code: Amended
    /// </summary>
    public const string LiteralAmended = "amended";

    /// <summary>
    /// Literal for code: Completed
    /// </summary>
    public const string LiteralCompleted = "completed";

    /// <summary>
    /// Literal for code: InProgress
    /// </summary>
    public const string LiteralInProgress = "in-progress";
  };
}
