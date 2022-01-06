// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The kind of medication order
  /// </summary>
  public static class MedicationRequestIntentCodes
  {
    /// <summary>
    /// The request represents an instance for the particular order, for example a medication administration record.
    /// </summary>
    public static readonly Coding InstanceOrder = new Coding
    {
      Code = "instance-order",
      Display = "Instance Order",
      System = "http://hl7.org/fhir/medication-request-intent"
    };
    /// <summary>
    /// The request represents a request/demand and authorization for action
    /// </summary>
    public static readonly Coding Order = new Coding
    {
      Code = "order",
      Display = "Order",
      System = "http://hl7.org/fhir/medication-request-intent"
    };
    /// <summary>
    /// The request represents an intension to ensure something occurs without providing an authorization for others to act
    /// </summary>
    public static readonly Coding Plan = new Coding
    {
      Code = "plan",
      Display = "Plan",
      System = "http://hl7.org/fhir/medication-request-intent"
    };
    /// <summary>
    /// The request is a suggestion made by someone/something that doesn't have an intention to ensure it occurs and without providing an authorization to act
    /// </summary>
    public static readonly Coding Proposal = new Coding
    {
      Code = "proposal",
      Display = "Proposal",
      System = "http://hl7.org/fhir/medication-request-intent"
    };

    /// <summary>
    /// Literal for code: InstanceOrder
    /// </summary>
    public const string LiteralInstanceOrder = "instance-order";

    /// <summary>
    /// Literal for code: Order
    /// </summary>
    public const string LiteralOrder = "order";

    /// <summary>
    /// Literal for code: Plan
    /// </summary>
    public const string LiteralPlan = "plan";

    /// <summary>
    /// Literal for code: Proposal
    /// </summary>
    public const string LiteralProposal = "proposal";
  };
}
