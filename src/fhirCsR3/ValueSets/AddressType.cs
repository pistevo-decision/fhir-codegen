// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// The type of an address (physical / postal)
  /// </summary>
  public static class AddressTypeCodes
  {
    /// <summary>
    /// An address that is both physical and postal.
    /// </summary>
    public static readonly Coding PostalAndPhysical = new Coding
    {
      Code = "both",
      Display = "Postal & Physical",
      System = "http://hl7.org/fhir/address-type"
    };
    /// <summary>
    /// A physical address that can be visited.
    /// </summary>
    public static readonly Coding Physical = new Coding
    {
      Code = "physical",
      Display = "Physical",
      System = "http://hl7.org/fhir/address-type"
    };
    /// <summary>
    /// Mailing addresses - PO Boxes and care-of addresses.
    /// </summary>
    public static readonly Coding Postal = new Coding
    {
      Code = "postal",
      Display = "Postal",
      System = "http://hl7.org/fhir/address-type"
    };

    /// <summary>
    /// Literal for code: PostalAndPhysical
    /// </summary>
    public const string LiteralPostalAndPhysical = "both";

    /// <summary>
    /// Literal for code: Physical
    /// </summary>
    public const string LiteralPhysical = "physical";

    /// <summary>
    /// Literal for code: Postal
    /// </summary>
    public const string LiteralPostal = "postal";
  };
}
