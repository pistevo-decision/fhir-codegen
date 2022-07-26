// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using fhirCsR4B.Models;

namespace fhirCsR4B.ValueSets
{
  /// <summary>
  /// Indicates whether the account is available to be used.
  /// </summary>
  public static class AccountStatusCodes
  {
    /// <summary>
    /// This account is active and may be used.
    /// </summary>
    public static readonly Coding Active = new Coding
    {
      Code = "active",
      Display = "Active",
      System = "http://hl7.org/fhir/account-status"
    };
    /// <summary>
    /// This instance should not have been part of this patient's medical record.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in error",
      System = "http://hl7.org/fhir/account-status"
    };
    /// <summary>
    /// This account is inactive and should not be used to track financial information.
    /// </summary>
    public static readonly Coding Inactive = new Coding
    {
      Code = "inactive",
      Display = "Inactive",
      System = "http://hl7.org/fhir/account-status"
    };
    /// <summary>
    /// This account is on hold.
    /// </summary>
    public static readonly Coding OnHold = new Coding
    {
      Code = "on-hold",
      Display = "On Hold",
      System = "http://hl7.org/fhir/account-status"
    };
    /// <summary>
    /// The account status is unknown.
    /// </summary>
    public static readonly Coding Unknown = new Coding
    {
      Code = "unknown",
      Display = "Unknown",
      System = "http://hl7.org/fhir/account-status"
    };

    /// <summary>
    /// Literal for code: Active
    /// </summary>
    public const string LiteralActive = "active";

    /// <summary>
    /// Literal for code: AccountStatusActive
    /// </summary>
    public const string LiteralAccountStatusActive = "http://hl7.org/fhir/account-status#active";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: AccountStatusEnteredInError
    /// </summary>
    public const string LiteralAccountStatusEnteredInError = "http://hl7.org/fhir/account-status#entered-in-error";

    /// <summary>
    /// Literal for code: Inactive
    /// </summary>
    public const string LiteralInactive = "inactive";

    /// <summary>
    /// Literal for code: AccountStatusInactive
    /// </summary>
    public const string LiteralAccountStatusInactive = "http://hl7.org/fhir/account-status#inactive";

    /// <summary>
    /// Literal for code: OnHold
    /// </summary>
    public const string LiteralOnHold = "on-hold";

    /// <summary>
    /// Literal for code: AccountStatusOnHold
    /// </summary>
    public const string LiteralAccountStatusOnHold = "http://hl7.org/fhir/account-status#on-hold";

    /// <summary>
    /// Literal for code: Unknown
    /// </summary>
    public const string LiteralUnknown = "unknown";

    /// <summary>
    /// Literal for code: AccountStatusUnknown
    /// </summary>
    public const string LiteralAccountStatusUnknown = "http://hl7.org/fhir/account-status#unknown";

    /// <summary>
    /// Dictionary for looking up AccountStatus Codings based on Codes
    /// </summary>
    public static Dictionary<string, Coding> Values = new Dictionary<string, Coding>() {
      { "active", Active }, 
      { "http://hl7.org/fhir/account-status#active", Active }, 
      { "entered-in-error", EnteredInError }, 
      { "http://hl7.org/fhir/account-status#entered-in-error", EnteredInError }, 
      { "inactive", Inactive }, 
      { "http://hl7.org/fhir/account-status#inactive", Inactive }, 
      { "on-hold", OnHold }, 
      { "http://hl7.org/fhir/account-status#on-hold", OnHold }, 
      { "unknown", Unknown }, 
      { "http://hl7.org/fhir/account-status#unknown", Unknown }, 
    };
  };
}