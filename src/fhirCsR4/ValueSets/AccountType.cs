// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This examples value set defines the set of codes that can be used to represent the type of an account.
  /// </summary>
  public static class AccountTypeCodes
  {
    /// <summary>
    /// An account represents a grouping of financial transactions that are tracked and reported together with a single balance. 	 	Examples of account codes (types) are Patient billing accounts (collection of charges), Cost centers; Cash.
    /// </summary>
    public static readonly Coding ActAccountCode = new Coding
    {
      Code = "_ActAccountCode",
      Display = "ActAccountCode",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// An account for collecting charges, reversals, adjustments and payments, including deductibles, copayments, coinsurance (financial transactions) credited or debited to the account receivable account for a patient's encounter.
    /// </summary>
    public static readonly Coding AccountReceivable = new Coding
    {
      Code = "ACCTRECEIVABLE",
      Display = "account receivable",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// American Express
    /// </summary>
    public static readonly Coding AmericanExpress = new Coding
    {
      Code = "AE",
      Display = "American Express",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Cash
    /// </summary>
    public static readonly Coding Cash = new Coding
    {
      Code = "CASH",
      Display = "Cash",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Description: Types of advance payment to be made on a plastic card usually issued by a financial institution used of purchasing services and/or products.
    /// </summary>
    public static readonly Coding CreditCard = new Coding
    {
      Code = "CC",
      Display = "credit card",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Diner's Club
    /// </summary>
    public static readonly Coding DinerQuoteSClub = new Coding
    {
      Code = "DN",
      Display = "Diner's Club",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Discover Card
    /// </summary>
    public static readonly Coding DiscoverCard = new Coding
    {
      Code = "DV",
      Display = "Discover Card",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Master Card
    /// </summary>
    public static readonly Coding MasterCard = new Coding
    {
      Code = "MC",
      Display = "Master Card",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// An account representing charges and credits (financial transactions) for a patient's encounter.
    /// </summary>
    public static readonly Coding PatientBillingAccount = new Coding
    {
      Code = "PBILLACCT",
      Display = "patient billing account",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };
    /// <summary>
    /// Visa
    /// </summary>
    public static readonly Coding Visa = new Coding
    {
      Code = "V",
      Display = "Visa",
      System = "http://terminology.hl7.org/CodeSystem/v3-ActCode"
    };

    /// <summary>
    /// Literal for code: ActAccountCode
    /// </summary>
    public const string LiteralActAccountCode = "_ActAccountCode";

    /// <summary>
    /// Literal for code: AccountReceivable
    /// </summary>
    public const string LiteralAccountReceivable = "ACCTRECEIVABLE";

    /// <summary>
    /// Literal for code: AmericanExpress
    /// </summary>
    public const string LiteralAmericanExpress = "AE";

    /// <summary>
    /// Literal for code: Cash
    /// </summary>
    public const string LiteralCash = "CASH";

    /// <summary>
    /// Literal for code: CreditCard
    /// </summary>
    public const string LiteralCreditCard = "CC";

    /// <summary>
    /// Literal for code: DinerQuoteSClub
    /// </summary>
    public const string LiteralDinerQuoteSClub = "DN";

    /// <summary>
    /// Literal for code: DiscoverCard
    /// </summary>
    public const string LiteralDiscoverCard = "DV";

    /// <summary>
    /// Literal for code: MasterCard
    /// </summary>
    public const string LiteralMasterCard = "MC";

    /// <summary>
    /// Literal for code: PatientBillingAccount
    /// </summary>
    public const string LiteralPatientBillingAccount = "PBILLACCT";

    /// <summary>
    /// Literal for code: Visa
    /// </summary>
    public const string LiteralVisa = "V";
  };
}
