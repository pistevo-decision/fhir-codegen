// <auto-generated />
// Built from: hl7.fhir.r3.core version: 3.0.2
  // Option: "NAMESPACE" = "fhirCsR3"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR3.Serialization;

namespace fhirCsR3.Models
{
  /// <summary>
  /// UDI may identify an unique instance of a device, or it may only identify the type of the device.  See [UDI mappings](device-mappings.html#udi) for a complete mapping of UDI parts to Device.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<DeviceUdi>))]
  public class DeviceUdi : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The AIDC form of UDIs should be scanned or otherwise used for the identification of the device whenever possible to minimize errors in records resulting from manual transcriptions. If separate barcodes for DI and PI are present, concatenate the string with DI first and in order of human readable expression on label.
    /// </summary>
    public byte[] CarrierAIDC { get; set; }
    /// <summary>
    /// If separate barcodes for DI and PI are present, concatenate the string with DI first and in order of human readable expression on label.
    /// </summary>
    public string CarrierHRF { get; set; }
    /// <summary>
    /// Extension container element for CarrierHRF
    /// </summary>
    public Element _CarrierHRF { get; set; }
    /// <summary>
    /// The device identifier (DI) is a mandatory, fixed portion of a UDI that identifies the labeler and the specific version or model of a device.
    /// </summary>
    public string DeviceIdentifier { get; set; }
    /// <summary>
    /// Extension container element for DeviceIdentifier
    /// </summary>
    public Element _DeviceIdentifier { get; set; }
    /// <summary>
    /// A coded entry to indicate how the data was entered.
    /// </summary>
    public string EntryType { get; set; }
    /// <summary>
    /// Extension container element for EntryType
    /// </summary>
    public Element _EntryType { get; set; }
    /// <summary>
    /// Organization that is charged with issuing UDIs for devices.  For example, the US FDA issuers include :
    /// 1) GS1: 
    /// http://hl7.org/fhir/NamingSystem/gs1-di, 
    /// 2) HIBCC:
    /// http://hl7.org/fhir/NamingSystem/hibcc-dI, 
    /// 3) ICCBBA for blood containers:
    /// http://hl7.org/fhir/NamingSystem/iccbba-blood-di, 
    /// 4) ICCBA for other devices:
    /// http://hl7.org/fhir/NamingSystem/iccbba-other-di.
    /// </summary>
    public string Issuer { get; set; }
    /// <summary>
    /// Extension container element for Issuer
    /// </summary>
    public Element _Issuer { get; set; }
    /// <summary>
    /// The identity of the authoritative source for UDI generation within a  jurisdiction.  All UDIs are globally unique within a single namespace. with the appropriate repository uri as the system.  For example,  UDIs of devices managed in the U.S. by the FDA, the value is  http://hl7.org/fhir/NamingSystem/fda-udi.
    /// </summary>
    public string Jurisdiction { get; set; }
    /// <summary>
    /// Extension container element for Jurisdiction
    /// </summary>
    public Element _Jurisdiction { get; set; }
    /// <summary>
    /// Use the `Device.type`  for a generic type or kind of device name.  Note the [GUDID](http://www.fda.gov/medicaldevices/deviceregulationandguidance/uniquedeviceidentification/globaludidatabasegudid/default.htm) lists the name as the 'Brand Name'.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(DeviceIdentifier))
      {
        writer.WriteString("deviceIdentifier", (string)DeviceIdentifier!);
      }

      if (_DeviceIdentifier != null)
      {
        writer.WritePropertyName("_deviceIdentifier");
        _DeviceIdentifier.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Name))
      {
        writer.WriteString("name", (string)Name!);
      }

      if (_Name != null)
      {
        writer.WritePropertyName("_name");
        _Name.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Jurisdiction))
      {
        writer.WriteString("jurisdiction", (string)Jurisdiction!);
      }

      if (_Jurisdiction != null)
      {
        writer.WritePropertyName("_jurisdiction");
        _Jurisdiction.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(CarrierHRF))
      {
        writer.WriteString("carrierHRF", (string)CarrierHRF!);
      }

      if (_CarrierHRF != null)
      {
        writer.WritePropertyName("_carrierHRF");
        _CarrierHRF.SerializeJson(writer, options);
      }

      if (CarrierAIDC != null)
      {
        writer.WriteString("carrierAIDC", System.Convert.ToBase64String(CarrierAIDC));
      }

      if (!string.IsNullOrEmpty(Issuer))
      {
        writer.WriteString("issuer", (string)Issuer!);
      }

      if (_Issuer != null)
      {
        writer.WritePropertyName("_issuer");
        _Issuer.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(EntryType))
      {
        writer.WriteString("entryType", (string)EntryType!);
      }

      if (_EntryType != null)
      {
        writer.WritePropertyName("_entryType");
        _EntryType.SerializeJson(writer, options);
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "carrierAIDC":
          CarrierAIDC = System.Convert.FromBase64String(reader.GetString());
          break;

        case "carrierHRF":
          CarrierHRF = reader.GetString();
          break;

        case "_carrierHRF":
          _CarrierHRF = new fhirCsR3.Models.Element();
          _CarrierHRF.DeserializeJson(ref reader, options);
          break;

        case "deviceIdentifier":
          DeviceIdentifier = reader.GetString();
          break;

        case "_deviceIdentifier":
          _DeviceIdentifier = new fhirCsR3.Models.Element();
          _DeviceIdentifier.DeserializeJson(ref reader, options);
          break;

        case "entryType":
          EntryType = reader.GetString();
          break;

        case "_entryType":
          _EntryType = new fhirCsR3.Models.Element();
          _EntryType.DeserializeJson(ref reader, options);
          break;

        case "issuer":
          Issuer = reader.GetString();
          break;

        case "_issuer":
          _Issuer = new fhirCsR3.Models.Element();
          _Issuer.DeserializeJson(ref reader, options);
          break;

        case "jurisdiction":
          Jurisdiction = reader.GetString();
          break;

        case "_jurisdiction":
          _Jurisdiction = new fhirCsR3.Models.Element();
          _Jurisdiction.DeserializeJson(ref reader, options);
          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR3.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the Device.udi.entryType field
  /// </summary>
  public static class DeviceUdiEntryTypeCodes {
    public const string BARCODE = "barcode";
    public const string RFID = "rfid";
    public const string MANUAL = "manual";
    public const string CARD = "card";
    public const string SELF_REPORTED = "self-reported";
    public const string UNKNOWN = "unknown";
  }
  /// <summary>
  /// This resource identifies an instance or a type of a manufactured item that is used in the provision of healthcare without being substantially changed through that activity. The device may be a medical or non-medical device.  Medical devices include durable (reusable) medical equipment, implantable devices, as well as disposable equipment used for diagnostic, treatment, and research for healthcare and public health.  Non-medical devices may include items such as a machine, cellphone, computer, application, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<Device>))]
  public class Device : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "Device";
    /// <summary>
    /// used for troubleshooting etc.
    /// </summary>
    public List<ContactPoint> Contact { get; set; }
    /// <summary>
    /// The date and time beyond which this device is no longer valid or should not be used (if applicable).
    /// </summary>
    public string ExpirationDate { get; set; }
    /// <summary>
    /// Extension container element for ExpirationDate
    /// </summary>
    public Element _ExpirationDate { get; set; }
    /// <summary>
    /// The barcode string from a barcode present on a device label or package may identify the instance, include names given to the device in local usage, or may identify the type of device. If the identifier identifies the type of device, Device.type element should be used.  For [UDI](device.html#5.11.3.2.2),  this element corresponds to the variable portion of the UDI that identifies the serial number of a specific device. See [UDI mappings](device-mappings.html#udi) for a complete mapping of UDI parts to Device.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// The place where the device can be found.
    /// </summary>
    public Reference Location { get; set; }
    /// <summary>
    /// Lot number assigned by the manufacturer.
    /// </summary>
    public string LotNumber { get; set; }
    /// <summary>
    /// Extension container element for LotNumber
    /// </summary>
    public Element _LotNumber { get; set; }
    /// <summary>
    /// The date and time when the device was manufactured.
    /// </summary>
    public string ManufactureDate { get; set; }
    /// <summary>
    /// Extension container element for ManufactureDate
    /// </summary>
    public Element _ManufactureDate { get; set; }
    /// <summary>
    /// A name of the manufacturer.
    /// </summary>
    public string Manufacturer { get; set; }
    /// <summary>
    /// Extension container element for Manufacturer
    /// </summary>
    public Element _Manufacturer { get; set; }
    /// <summary>
    /// The "model" is an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    /// </summary>
    public string Model { get; set; }
    /// <summary>
    /// Extension container element for Model
    /// </summary>
    public Element _Model { get; set; }
    /// <summary>
    /// Descriptive information, usage information or implantation information that is not captured in an existing element.
    /// </summary>
    public List<Annotation> Note { get; set; }
    /// <summary>
    /// An organization that is responsible for the provision and ongoing maintenance of the device.
    /// </summary>
    public Reference Owner { get; set; }
    /// <summary>
    /// Patient information, If the device is affixed to a person.
    /// </summary>
    public Reference Patient { get; set; }
    /// <summary>
    /// Provides additional safety characteristics about a medical device.  For example devices containing latex.
    /// </summary>
    public List<CodeableConcept> Safety { get; set; }
    /// <summary>
    /// This element is labeled as a modifier because the status contains the codes inactive and entered-in-error that mark the device (record)as not currently valid.
    /// </summary>
    public string Status { get; set; }
    /// <summary>
    /// Extension container element for Status
    /// </summary>
    public Element _Status { get; set; }
    /// <summary>
    /// Code or identifier to identify a kind of device.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// UDI may identify an unique instance of a device, or it may only identify the type of the device.  See [UDI mappings](device-mappings.html#udi) for a complete mapping of UDI parts to Device.
    /// </summary>
    public DeviceUdi Udi { get; set; }
    /// <summary>
    /// If the device is running a FHIR server, the network address should  be the Base URL from which a conformance statement may be retrieved.
    /// </summary>
    public string Url { get; set; }
    /// <summary>
    /// Extension container element for Url
    /// </summary>
    public Element _Url { get; set; }
    /// <summary>
    /// The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    /// </summary>
    public string Version { get; set; }
    /// <summary>
    /// Extension container element for Version
    /// </summary>
    public Element _Version { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      if (!string.IsNullOrEmpty(ResourceType))
      {
        writer.WriteString("resourceType", (string)ResourceType!);
      }


      ((fhirCsR3.Models.DomainResource)this).SerializeJson(writer, options, false);

      if ((Identifier != null) && (Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();

        foreach (Identifier valIdentifier in Identifier)
        {
          valIdentifier.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Udi != null)
      {
        writer.WritePropertyName("udi");
        Udi.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Status))
      {
        writer.WriteString("status", (string)Status!);
      }

      if (_Status != null)
      {
        writer.WritePropertyName("_status");
        _Status.SerializeJson(writer, options);
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(LotNumber))
      {
        writer.WriteString("lotNumber", (string)LotNumber!);
      }

      if (_LotNumber != null)
      {
        writer.WritePropertyName("_lotNumber");
        _LotNumber.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Manufacturer))
      {
        writer.WriteString("manufacturer", (string)Manufacturer!);
      }

      if (_Manufacturer != null)
      {
        writer.WritePropertyName("_manufacturer");
        _Manufacturer.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ManufactureDate))
      {
        writer.WriteString("manufactureDate", (string)ManufactureDate!);
      }

      if (_ManufactureDate != null)
      {
        writer.WritePropertyName("_manufactureDate");
        _ManufactureDate.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(ExpirationDate))
      {
        writer.WriteString("expirationDate", (string)ExpirationDate!);
      }

      if (_ExpirationDate != null)
      {
        writer.WritePropertyName("_expirationDate");
        _ExpirationDate.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Model))
      {
        writer.WriteString("model", (string)Model!);
      }

      if (_Model != null)
      {
        writer.WritePropertyName("_model");
        _Model.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Version))
      {
        writer.WriteString("version", (string)Version!);
      }

      if (_Version != null)
      {
        writer.WritePropertyName("_version");
        _Version.SerializeJson(writer, options);
      }

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
      }

      if (Owner != null)
      {
        writer.WritePropertyName("owner");
        Owner.SerializeJson(writer, options);
      }

      if ((Contact != null) && (Contact.Count != 0))
      {
        writer.WritePropertyName("contact");
        writer.WriteStartArray();

        foreach (ContactPoint valContact in Contact)
        {
          valContact.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Location != null)
      {
        writer.WritePropertyName("location");
        Location.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Url))
      {
        writer.WriteString("url", (string)Url!);
      }

      if (_Url != null)
      {
        writer.WritePropertyName("_url");
        _Url.SerializeJson(writer, options);
      }

      if ((Note != null) && (Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();

        foreach (Annotation valNote in Note)
        {
          valNote.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Safety != null) && (Safety.Count != 0))
      {
        writer.WritePropertyName("safety");
        writer.WriteStartArray();

        foreach (CodeableConcept valSafety in Safety)
        {
          valSafety.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (includeStartObject)
      {
        writer.WriteEndObject();
      }
    }
    /// <summary>
    /// Deserialize a JSON property
    /// </summary>
    public new void DeserializeJsonProperty(ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "contact":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Contact = new List<ContactPoint>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ContactPoint objContact = new fhirCsR3.Models.ContactPoint();
            objContact.DeserializeJson(ref reader, options);
            Contact.Add(objContact);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Contact.Count == 0)
          {
            Contact = null;
          }

          break;

        case "expirationDate":
          ExpirationDate = reader.GetString();
          break;

        case "_expirationDate":
          _ExpirationDate = new fhirCsR3.Models.Element();
          _ExpirationDate.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Identifier objIdentifier = new fhirCsR3.Models.Identifier();
            objIdentifier.DeserializeJson(ref reader, options);
            Identifier.Add(objIdentifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Identifier.Count == 0)
          {
            Identifier = null;
          }

          break;

        case "location":
          Location = new fhirCsR3.Models.Reference();
          Location.DeserializeJson(ref reader, options);
          break;

        case "lotNumber":
          LotNumber = reader.GetString();
          break;

        case "_lotNumber":
          _LotNumber = new fhirCsR3.Models.Element();
          _LotNumber.DeserializeJson(ref reader, options);
          break;

        case "manufactureDate":
          ManufactureDate = reader.GetString();
          break;

        case "_manufactureDate":
          _ManufactureDate = new fhirCsR3.Models.Element();
          _ManufactureDate.DeserializeJson(ref reader, options);
          break;

        case "manufacturer":
          Manufacturer = reader.GetString();
          break;

        case "_manufacturer":
          _Manufacturer = new fhirCsR3.Models.Element();
          _Manufacturer.DeserializeJson(ref reader, options);
          break;

        case "model":
          Model = reader.GetString();
          break;

        case "_model":
          _Model = new fhirCsR3.Models.Element();
          _Model.DeserializeJson(ref reader, options);
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Annotation objNote = new fhirCsR3.Models.Annotation();
            objNote.DeserializeJson(ref reader, options);
            Note.Add(objNote);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Note.Count == 0)
          {
            Note = null;
          }

          break;

        case "owner":
          Owner = new fhirCsR3.Models.Reference();
          Owner.DeserializeJson(ref reader, options);
          break;

        case "patient":
          Patient = new fhirCsR3.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        case "safety":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Safety = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.CodeableConcept objSafety = new fhirCsR3.Models.CodeableConcept();
            objSafety.DeserializeJson(ref reader, options);
            Safety.Add(objSafety);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Safety.Count == 0)
          {
            Safety = null;
          }

          break;

        case "status":
          Status = reader.GetString();
          break;

        case "_status":
          _Status = new fhirCsR3.Models.Element();
          _Status.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR3.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        case "udi":
          Udi = new fhirCsR3.Models.DeviceUdi();
          Udi.DeserializeJson(ref reader, options);
          break;

        case "url":
          Url = reader.GetString();
          break;

        case "_url":
          _Url = new fhirCsR3.Models.Element();
          _Url.DeserializeJson(ref reader, options);
          break;

        case "version":
          Version = reader.GetString();
          break;

        case "_version":
          _Version = new fhirCsR3.Models.Element();
          _Version.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Deserialize a JSON object
    /// </summary>
    public new void DeserializeJson(ref Utf8JsonReader reader, JsonSerializerOptions options)
    {
      string propertyName;

      while (reader.Read())
      {
        if (reader.TokenType == JsonTokenType.EndObject)
        {
          return;
        }

        if (reader.TokenType == JsonTokenType.PropertyName)
        {
          propertyName = reader.GetString();
          reader.Read();
          this.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }
  }
  /// <summary>
  /// Code Values for the Device.status field
  /// </summary>
  public static class DeviceStatusCodes {
    public const string ACTIVE = "active";
    public const string INACTIVE = "inactive";
    public const string ENTERED_IN_ERROR = "entered-in-error";
    public const string UNKNOWN = "unknown";
  }
}
