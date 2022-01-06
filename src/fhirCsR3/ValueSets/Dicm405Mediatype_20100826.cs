// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using fhirCsR3.Models;

namespace fhirCsR3.ValueSets
{
  /// <summary>
  /// Media Type Code
  /// </summary>
  public static class Dicm405MediatypeCodes
  {
    /// <summary>
    /// Film type of output
    /// </summary>
    public static readonly Coding Film = new Coding
    {
      Code = "110010",
      Display = "Film",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// A device that connects using the USB hard drive interface. These may be USB-Sticks, portable hard drives, and other technologies
    /// </summary>
    public static readonly Coding USBDiskEmulation = new Coding
    {
      Code = "110030",
      Display = "USB Disk Emulation",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Email and email attachments used as a media for data transport
    /// </summary>
    public static readonly Coding Email = new Coding
    {
      Code = "110031",
      Display = "Email",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// CD-R, CD-ROM, and CD-RW media used for data transport
    /// </summary>
    public static readonly Coding CD = new Coding
    {
      Code = "110032",
      Display = "CD",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// DVD, DVD-RAM, and other DVD formatted media used for data transport
    /// </summary>
    public static readonly Coding DVD = new Coding
    {
      Code = "110033",
      Display = "DVD",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Media that comply with the Compact Flash standard
    /// </summary>
    public static readonly Coding CompactFlash = new Coding
    {
      Code = "110034",
      Display = "Compact Flash",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Media that comply with the Multi-media Card standard
    /// </summary>
    public static readonly Coding MultiMediaCard = new Coding
    {
      Code = "110035",
      Display = "Multi-media Card",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Media that comply with the Secure Digital Card standard
    /// </summary>
    public static readonly Coding SecureDigitalCard = new Coding
    {
      Code = "110036",
      Display = "Secure Digital Card",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// URI Identifier for network or other resource, see RFC 3968
    /// </summary>
    public static readonly Coding URI = new Coding
    {
      Code = "110037",
      Display = "URI",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };
    /// <summary>
    /// Any paper or similar document
    /// </summary>
    public static readonly Coding PaperDocument = new Coding
    {
      Code = "110038",
      Display = "Paper Document",
      System = "http://dicom.nema.org/resources/ontology/DCM"
    };

    /// <summary>
    /// Literal for code: Film
    /// </summary>
    public const string LiteralFilm = "110010";

    /// <summary>
    /// Literal for code: USBDiskEmulation
    /// </summary>
    public const string LiteralUSBDiskEmulation = "110030";

    /// <summary>
    /// Literal for code: Email
    /// </summary>
    public const string LiteralEmail = "110031";

    /// <summary>
    /// Literal for code: CD
    /// </summary>
    public const string LiteralCD = "110032";

    /// <summary>
    /// Literal for code: DVD
    /// </summary>
    public const string LiteralDVD = "110033";

    /// <summary>
    /// Literal for code: CompactFlash
    /// </summary>
    public const string LiteralCompactFlash = "110034";

    /// <summary>
    /// Literal for code: MultiMediaCard
    /// </summary>
    public const string LiteralMultiMediaCard = "110035";

    /// <summary>
    /// Literal for code: SecureDigitalCard
    /// </summary>
    public const string LiteralSecureDigitalCard = "110036";

    /// <summary>
    /// Literal for code: URI
    /// </summary>
    public const string LiteralURI = "110037";

    /// <summary>
    /// Literal for code: PaperDocument
    /// </summary>
    public const string LiteralPaperDocument = "110038";
  };
}
