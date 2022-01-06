// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using fhirCsR4.Models;

namespace fhirCsR4.ValueSets
{
  /// <summary>
  /// This example value set defines a set of codes that can be used to indicate the purpose for which you would contact a contact party.
  /// </summary>
  public static class ContactentityTypeCodes
  {
    /// <summary>
    /// Contact details for administrative enquiries.
    /// </summary>
    public static readonly Coding Administrative = new Coding
    {
      Code = "ADMIN",
      Display = "Administrative",
      System = "http://terminology.hl7.org/CodeSystem/contactentity-type"
    };
    /// <summary>
    /// Contact details for information regarding to billing/general finance enquiries.
    /// </summary>
    public static readonly Coding Billing = new Coding
    {
      Code = "BILL",
      Display = "Billing",
      System = "http://terminology.hl7.org/CodeSystem/contactentity-type"
    };
    /// <summary>
    /// Contact details for issues related to Human Resources, such as staff matters, OH&amp;S etc.
    /// </summary>
    public static readonly Coding HumanResource = new Coding
    {
      Code = "HR",
      Display = "Human Resource",
      System = "http://terminology.hl7.org/CodeSystem/contactentity-type"
    };
    /// <summary>
    /// Generic information contact for patients.
    /// </summary>
    public static readonly Coding Patient = new Coding
    {
      Code = "PATINF",
      Display = "Patient",
      System = "http://terminology.hl7.org/CodeSystem/contactentity-type"
    };
    /// <summary>
    /// Contact details for dealing with issues related to insurance claims/adjudication/payment.
    /// </summary>
    public static readonly Coding Payor = new Coding
    {
      Code = "PAYOR",
      Display = "Payor",
      System = "http://terminology.hl7.org/CodeSystem/contactentity-type"
    };
    /// <summary>
    /// Dedicated contact point for matters relating to press enquiries.
    /// </summary>
    public static readonly Coding Press = new Coding
    {
      Code = "PRESS",
      Display = "Press",
      System = "http://terminology.hl7.org/CodeSystem/contactentity-type"
    };

    /// <summary>
    /// Literal for code: Administrative
    /// </summary>
    public const string LiteralAdministrative = "ADMIN";

    /// <summary>
    /// Literal for code: Billing
    /// </summary>
    public const string LiteralBilling = "BILL";

    /// <summary>
    /// Literal for code: HumanResource
    /// </summary>
    public const string LiteralHumanResource = "HR";

    /// <summary>
    /// Literal for code: Patient
    /// </summary>
    public const string LiteralPatient = "PATINF";

    /// <summary>
    /// Literal for code: Payor
    /// </summary>
    public const string LiteralPayor = "PAYOR";

    /// <summary>
    /// Literal for code: Press
    /// </summary>
    public const string LiteralPress = "PRESS";
  };
}
