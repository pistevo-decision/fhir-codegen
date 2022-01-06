// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Identifies the purpose for this identifier, if known .
  /// </summary>
  public static class IdentifierUseCodes
  {
    /// <summary>
    /// The identifier considered to be most trusted for the identification of this item.
    /// </summary>
    public static readonly Coding Official = new Coding
    {
      Code = "official",
      Display = "Official",
      System = "http://hl7.org/fhir/identifier-use"
    };
    /// <summary>
    /// An identifier that was assigned in secondary use - it serves to identify the object in a relative context, but cannot be consistently assigned to the same object again in a different context.
    /// </summary>
    public static readonly Coding Secondary = new Coding
    {
      Code = "secondary",
      Display = "Secondary",
      System = "http://hl7.org/fhir/identifier-use"
    };
    /// <summary>
    /// A temporary identifier.
    /// </summary>
    public static readonly Coding Temp = new Coding
    {
      Code = "temp",
      Display = "Temp",
      System = "http://hl7.org/fhir/identifier-use"
    };
    /// <summary>
    /// The identifier recommended for display and use in real-world interactions.
    /// </summary>
    public static readonly Coding Usual = new Coding
    {
      Code = "usual",
      Display = "Usual",
      System = "http://hl7.org/fhir/identifier-use"
    };

    /// <summary>
    /// Literal for code: Official
    /// </summary>
    public const string LiteralOfficial = "official";

    /// <summary>
    /// Literal for code: Secondary
    /// </summary>
    public const string LiteralSecondary = "secondary";

    /// <summary>
    /// Literal for code: Temp
    /// </summary>
    public const string LiteralTemp = "temp";

    /// <summary>
    /// Literal for code: Usual
    /// </summary>
    public const string LiteralUsual = "usual";
  };
}
