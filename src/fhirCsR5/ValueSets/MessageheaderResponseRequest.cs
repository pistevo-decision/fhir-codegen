// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// HL7-defined table of codes which identify conditions under which acknowledgments are required to be returned in response to a message.
  /// </summary>
  public static class MessageheaderResponseRequestCodes
  {
    /// <summary>
    /// initiator expects a response for this message.
    /// </summary>
    public static readonly Coding Always = new Coding
    {
      Code = "always",
      Display = "Always",
      System = "http://hl7.org/fhir/messageheader-response-request"
    };
    /// <summary>
    /// initiator does not expect a response.
    /// </summary>
    public static readonly Coding Never = new Coding
    {
      Code = "never",
      Display = "Never",
      System = "http://hl7.org/fhir/messageheader-response-request"
    };
    /// <summary>
    /// initiator expects a response only if in error.
    /// </summary>
    public static readonly Coding ErrorRejectConditionsOnly = new Coding
    {
      Code = "on-error",
      Display = "Error/reject conditions only",
      System = "http://hl7.org/fhir/messageheader-response-request"
    };
    /// <summary>
    /// initiator expects a response only if successful.
    /// </summary>
    public static readonly Coding SuccessfulCompletionOnly = new Coding
    {
      Code = "on-success",
      Display = "Successful completion only",
      System = "http://hl7.org/fhir/messageheader-response-request"
    };

    /// <summary>
    /// Literal for code: Always
    /// </summary>
    public const string LiteralAlways = "always";

    /// <summary>
    /// Literal for code: MessageheaderResponseRequestAlways
    /// </summary>
    public const string LiteralMessageheaderResponseRequestAlways = "http://hl7.org/fhir/messageheader-response-request#always";

    /// <summary>
    /// Literal for code: Never
    /// </summary>
    public const string LiteralNever = "never";

    /// <summary>
    /// Literal for code: MessageheaderResponseRequestNever
    /// </summary>
    public const string LiteralMessageheaderResponseRequestNever = "http://hl7.org/fhir/messageheader-response-request#never";

    /// <summary>
    /// Literal for code: ErrorRejectConditionsOnly
    /// </summary>
    public const string LiteralErrorRejectConditionsOnly = "on-error";

    /// <summary>
    /// Literal for code: MessageheaderResponseRequestErrorRejectConditionsOnly
    /// </summary>
    public const string LiteralMessageheaderResponseRequestErrorRejectConditionsOnly = "http://hl7.org/fhir/messageheader-response-request#on-error";

    /// <summary>
    /// Literal for code: SuccessfulCompletionOnly
    /// </summary>
    public const string LiteralSuccessfulCompletionOnly = "on-success";

    /// <summary>
    /// Literal for code: MessageheaderResponseRequestSuccessfulCompletionOnly
    /// </summary>
    public const string LiteralMessageheaderResponseRequestSuccessfulCompletionOnly = "http://hl7.org/fhir/messageheader-response-request#on-success";

    /// <summary>
    /// Dictionary for looking up MessageheaderResponseRequest Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "always", Always }, 
      { "http://hl7.org/fhir/messageheader-response-request#always", Always }, 
      { "never", Never }, 
      { "http://hl7.org/fhir/messageheader-response-request#never", Never }, 
      { "on-error", ErrorRejectConditionsOnly }, 
      { "http://hl7.org/fhir/messageheader-response-request#on-error", ErrorRejectConditionsOnly }, 
      { "on-success", SuccessfulCompletionOnly }, 
      { "http://hl7.org/fhir/messageheader-response-request#on-success", SuccessfulCompletionOnly }, 
    };
  };
}