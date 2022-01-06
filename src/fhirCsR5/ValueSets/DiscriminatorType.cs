// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using fhirCsR5.Models;

namespace fhirCsR5.ValueSets
{
  /// <summary>
  /// How an element value is interpreted when discrimination is evaluated.
  /// </summary>
  public static class DiscriminatorTypeCodes
  {
    /// <summary>
    /// The slices are differentiated by the presence or absence of the nominated element.
    /// </summary>
    public static readonly Coding Exists = new Coding
    {
      Code = "exists",
      Display = "Exists",
      System = "http://hl7.org/fhir/discriminator-type"
    };
    /// <summary>
    /// The slices have different values in the nominated element, as determined by testing them against the applicable ElementDefinition.pattern[x].
    /// </summary>
    public static readonly Coding Pattern = new Coding
    {
      Code = "pattern",
      Display = "Pattern",
      System = "http://hl7.org/fhir/discriminator-type"
    };
    /// <summary>
    /// The slices are differentiated by conformance of the nominated element to a specified profile. Note that if the path specifies .resolve() then the profile is the target profile on the reference. In this case, validation by the possible profiles is required to differentiate the slices.
    /// </summary>
    public static readonly Coding Profile = new Coding
    {
      Code = "profile",
      Display = "Profile",
      System = "http://hl7.org/fhir/discriminator-type"
    };
    /// <summary>
    /// The slices are differentiated by type of the nominated element.
    /// </summary>
    public static readonly Coding Type = new Coding
    {
      Code = "type",
      Display = "Type",
      System = "http://hl7.org/fhir/discriminator-type"
    };
    /// <summary>
    /// The slices have different values in the nominated element.
    /// </summary>
    public static readonly Coding Value = new Coding
    {
      Code = "value",
      Display = "Value",
      System = "http://hl7.org/fhir/discriminator-type"
    };

    /// <summary>
    /// Literal for code: Exists
    /// </summary>
    public const string LiteralExists = "exists";

    /// <summary>
    /// Literal for code: Pattern
    /// </summary>
    public const string LiteralPattern = "pattern";

    /// <summary>
    /// Literal for code: Profile
    /// </summary>
    public const string LiteralProfile = "profile";

    /// <summary>
    /// Literal for code: Type
    /// </summary>
    public const string LiteralType = "type";

    /// <summary>
    /// Literal for code: Value
    /// </summary>
    public const string LiteralValue = "value";
  };
}
