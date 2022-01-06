// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This value set includes sample Payee Type codes.
  /// </summary>
  public static class PayeetypeCodes
  {
    /// <summary>
    /// Any benefit payable will be paid to a third party such as a guarrantor.
    /// </summary>
    public static readonly Coding Provider = new Coding
    {
      Code = "other",
      Display = "Provider",
      System = "http://terminology.hl7.org/CodeSystem/payeetype"
    };
    /// <summary>
    /// Any benefit payable will be paid to the provider (Assignment of Benefit).
    /// </summary>
    public static readonly Coding Provider_2 = new Coding
    {
      Code = "provider",
      Display = "Provider",
      System = "http://terminology.hl7.org/CodeSystem/payeetype"
    };
    /// <summary>
    /// The subscriber (policy holder) will be reimbursed.
    /// </summary>
    public static readonly Coding Subscriber = new Coding
    {
      Code = "subscriber",
      Display = "Subscriber",
      System = "http://terminology.hl7.org/CodeSystem/payeetype"
    };

    /// <summary>
    /// Literal for code: Provider
    /// </summary>
    public const string LiteralProvider = "other";

    /// <summary>
    /// Literal for code: Provider_2
    /// </summary>
    public const string LiteralProvider_2 = "provider";

    /// <summary>
    /// Literal for code: Subscriber
    /// </summary>
    public const string LiteralSubscriber = "subscriber";
  };
}
