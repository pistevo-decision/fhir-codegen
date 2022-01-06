// <auto-generated />
// Built from: hl7.fhir.r2.core version: 1.0.2
  // Option: "NAMESPACE" = "fhirCsR2"

using fhirCsR2.Models;

namespace fhirCsR2.ValueSets
{
  /// <summary>
  /// The use of a human name
  /// </summary>
  public static class NameUseCodes
  {
    /// <summary>
    /// Anonymous assigned name, alias, or pseudonym (used to protect a person's identity for privacy reasons)
    /// </summary>
    public static readonly Coding Anonymous = new Coding
    {
      Code = "anonymous",
      Display = "Anonymous",
      System = "http://hl7.org/fhir/name-use"
    };
    /// <summary>
    /// A name used prior to marriage. Marriage naming customs vary greatly around the world. This name use is for use by applications that collect and store "maiden" names. Though the concept of maiden name is often gender specific, the use of this term is not gender specific. The use of this term does not imply any particular history for a person's name, nor should the maiden name be determined algorithmically.
    /// </summary>
    public static readonly Coding Maiden = new Coding
    {
      Code = "maiden",
      Display = "Maiden",
      System = "http://hl7.org/fhir/name-use"
    };
    /// <summary>
    /// A name that is used to address the person in an informal manner, but is not part of their formal or usual name
    /// </summary>
    public static readonly Coding Nickname = new Coding
    {
      Code = "nickname",
      Display = "Nickname",
      System = "http://hl7.org/fhir/name-use"
    };
    /// <summary>
    /// The formal name as registered in an official (government) registry, but which name might not be commonly used. May be called "legal name".
    /// </summary>
    public static readonly Coding Official = new Coding
    {
      Code = "official",
      Display = "Official",
      System = "http://hl7.org/fhir/name-use"
    };
    /// <summary>
    /// This name is no longer in use (or was never correct, but retained for records)
    /// </summary>
    public static readonly Coding Old = new Coding
    {
      Code = "old",
      Display = "Old",
      System = "http://hl7.org/fhir/name-use"
    };
    /// <summary>
    /// A temporary name. Name.period can provide more detailed information. This may also be used for temporary names assigned at birth or in emergency situations.
    /// </summary>
    public static readonly Coding Temp = new Coding
    {
      Code = "temp",
      Display = "Temp",
      System = "http://hl7.org/fhir/name-use"
    };
    /// <summary>
    /// Known as/conventional/the one you normally use
    /// </summary>
    public static readonly Coding Usual = new Coding
    {
      Code = "usual",
      Display = "Usual",
      System = "http://hl7.org/fhir/name-use"
    };

    /// <summary>
    /// Literal for code: Anonymous
    /// </summary>
    public const string LiteralAnonymous = "anonymous";

    /// <summary>
    /// Literal for code: Maiden
    /// </summary>
    public const string LiteralMaiden = "maiden";

    /// <summary>
    /// Literal for code: Nickname
    /// </summary>
    public const string LiteralNickname = "nickname";

    /// <summary>
    /// Literal for code: Official
    /// </summary>
    public const string LiteralOfficial = "official";

    /// <summary>
    /// Literal for code: Old
    /// </summary>
    public const string LiteralOld = "old";

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
