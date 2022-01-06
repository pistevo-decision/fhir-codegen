// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set contract specific codes for status.
  /// </summary>
  public static class ContractPublicationstatusCodes
  {
    /// <summary>
    /// Contract is augmented with additional information to correct errors in a predecessor or to updated values in a predecessor. Usage: Contract altered within effective time. Precedence Order = 9. Comparable FHIR and v.3 status codes: revised; replaced.
    /// </summary>
    public static readonly Coding Amended = new Coding
    {
      Code = "amended",
      Display = "Amended",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract is augmented with additional information that was missing from a predecessor Contract. Usage: Contract altered within effective time. Precedence Order = 9. Comparable FHIR and v.3 status codes: updated, replaced.
    /// </summary>
    public static readonly Coding Appended = new Coding
    {
      Code = "appended",
      Display = "Appended",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract is terminated due to failure of the Grantor and/or the Grantee to fulfil one or more contract provisions. Usage: Abnormal contract termination. Precedence Order = 10. Comparable FHIR and v.3 status codes: stopped; failed; aborted.
    /// </summary>
    public static readonly Coding Cancelled = new Coding
    {
      Code = "cancelled",
      Display = "Cancelled",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract is pended to rectify failure of the Grantor or the Grantee to fulfil contract provision(s). E.g., Grantee complaint about Grantor's failure to comply with contract provisions. Usage: Contract pended. Precedence Order = 7. Comparable FHIR and v.3 status codes: on hold; pended; suspended.
    /// </summary>
    public static readonly Coding Disputed = new Coding
    {
      Code = "disputed",
      Display = "Disputed",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract was created in error. No Precedence Order.  Status may be applied to a Contract with any status.
    /// </summary>
    public static readonly Coding EnteredInError = new Coding
    {
      Code = "entered-in-error",
      Display = "Entered in Error",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract execution pending; may be executed when either the Grantor or the Grantee accepts the contract provisions by signing. I.e., where either the Grantor or the Grantee has signed, but not both. E.g., when an insurance applicant signs the insurers application, which references the policy. Usage: Optional first step of contract execution activity.  May be skipped and contracting activity moves directly to executed state. Precedence Order = 3. Comparable FHIR and v.3 status codes: draft; preliminary; planned; intended; active.
    /// </summary>
    public static readonly Coding Executable = new Coding
    {
      Code = "executable",
      Display = "Executable",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract is activated for period stipulated when both the Grantor and Grantee have signed it. Usage: Required state for normal completion of contracting activity.  Precedence Order = 6. Comparable FHIR and v.3 status codes: accepted; completed.
    /// </summary>
    public static readonly Coding Executed = new Coding
    {
      Code = "executed",
      Display = "Executed",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract execution is suspended while either or both the Grantor and Grantee propose and consider new or revised contract provisions. I.e., where the party which has not signed proposes changes to the terms.  E .g., a life insurer declines to agree to the signed application because the life insurer has evidence that the applicant, who asserted to being younger or a non-smoker to get a lower premium rate - but offers instead to agree to a higher premium based on the applicants actual age or smoking status. Usage: Optional contract activity between executable and executed state. Precedence Order = 4. Comparable FHIR and v.3 status codes: in progress; review; held.
    /// </summary>
    public static readonly Coding Negotiable = new Coding
    {
      Code = "negotiable",
      Display = "Negotiable",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract is a proposal by either the Grantor or the Grantee. Aka - A Contract hard copy or electronic 'template', 'form' or 'application'. E.g., health insurance application; consent directive form. Usage: Beginning of contract negotiation, which may have been completed as a precondition because used for 0..* contracts. Precedence Order = 2. Comparable FHIR and v.3 status codes: requested; new.
    /// </summary>
    public static readonly Coding Offered = new Coding
    {
      Code = "offered",
      Display = "Offered",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract template is available as the basis for an application or offer by the Grantor or Grantee. E.g., health insurance policy; consent directive policy.  Usage: Required initial contract activity, which may have been completed as a precondition because used for 0..* contracts. Precedence Order = 1. Comparable FHIR and v.3 status codes: proposed; intended.
    /// </summary>
    public static readonly Coding Policy = new Coding
    {
      Code = "policy",
      Display = "Policy",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    ///  Execution of the Contract is not completed because either or both the Grantor and Grantee decline to accept some or all of the contract provisions. Usage: Optional contract activity between executable and abnormal termination. Precedence Order = 5. Comparable FHIR and v.3 status codes:  stopped; cancelled.
    /// </summary>
    public static readonly Coding Rejected = new Coding
    {
      Code = "rejected",
      Display = "Rejected",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Beginning of a successor Contract at the termination of predecessor Contract lifecycle. Usage: Follows termination of a preceding Contract that has reached its expiry date. Precedence Order = 13. Comparable FHIR and v.3 status codes: superseded.
    /// </summary>
    public static readonly Coding Renewed = new Coding
    {
      Code = "renewed",
      Display = "Renewed",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract is reactivated after being pended because of faulty execution. *E.g., competency of the signer(s), or where the policy is substantially different from and did not accompany the application/form so that the applicant could not compare them. Aka - ''reactivated''. Usage: Optional stage where a pended contract is reactivated. Precedence Order = 8. Comparable FHIR and v.3 status codes: reactivated.
    /// </summary>
    public static readonly Coding Resolved = new Coding
    {
      Code = "resolved",
      Display = "Resolved",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// A Contract that is rescinded.  May be required prior to replacing with an updated Contract. Comparable FHIR and v.3 status codes: nullified.
    /// </summary>
    public static readonly Coding Revoked = new Coding
    {
      Code = "revoked",
      Display = "Revoked",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };
    /// <summary>
    /// Contract reaches its expiry date. It might or might not be renewed or renegotiated. Usage: Normal end of contract period. Precedence Order = 12. Comparable FHIR and v.3 status codes: Obsoleted.
    /// </summary>
    public static readonly Coding Terminated = new Coding
    {
      Code = "terminated",
      Display = "Terminated",
      System = "http://hl7.org/fhir/contract-publicationstatus"
    };

    /// <summary>
    /// Literal for code: Amended
    /// </summary>
    public const string LiteralAmended = "amended";

    /// <summary>
    /// Literal for code: Appended
    /// </summary>
    public const string LiteralAppended = "appended";

    /// <summary>
    /// Literal for code: Cancelled
    /// </summary>
    public const string LiteralCancelled = "cancelled";

    /// <summary>
    /// Literal for code: Disputed
    /// </summary>
    public const string LiteralDisputed = "disputed";

    /// <summary>
    /// Literal for code: EnteredInError
    /// </summary>
    public const string LiteralEnteredInError = "entered-in-error";

    /// <summary>
    /// Literal for code: Executable
    /// </summary>
    public const string LiteralExecutable = "executable";

    /// <summary>
    /// Literal for code: Executed
    /// </summary>
    public const string LiteralExecuted = "executed";

    /// <summary>
    /// Literal for code: Negotiable
    /// </summary>
    public const string LiteralNegotiable = "negotiable";

    /// <summary>
    /// Literal for code: Offered
    /// </summary>
    public const string LiteralOffered = "offered";

    /// <summary>
    /// Literal for code: Policy
    /// </summary>
    public const string LiteralPolicy = "policy";

    /// <summary>
    /// Literal for code: Rejected
    /// </summary>
    public const string LiteralRejected = "rejected";

    /// <summary>
    /// Literal for code: Renewed
    /// </summary>
    public const string LiteralRenewed = "renewed";

    /// <summary>
    /// Literal for code: Resolved
    /// </summary>
    public const string LiteralResolved = "resolved";

    /// <summary>
    /// Literal for code: Revoked
    /// </summary>
    public const string LiteralRevoked = "revoked";

    /// <summary>
    /// Literal for code: Terminated
    /// </summary>
    public const string LiteralTerminated = "terminated";
  };
}
