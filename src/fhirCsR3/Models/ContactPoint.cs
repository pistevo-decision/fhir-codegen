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
  /// Details for all kinds of technology mediated contact points for a person or organization, including telephone, email, etc.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ContactPoint>))]
  public class ContactPoint : Element,  IFhirJsonSerializable {
    /// <summary>
    /// Time period when the contact point was/is in use.
    /// </summary>
    public Period Period { get; set; }
    /// <summary>
    /// Note that rank does not necessarily follow the order in which the contacts are represented in the instance.
    /// </summary>
    public uint? Rank { get; set; }
    /// <summary>
    /// Telecommunications form for contact point - what communications system is required to make use of the contact.
    /// </summary>
    public string System { get; set; }
    /// <summary>
    /// Extension container element for System
    /// </summary>
    public Element _System { get; set; }
    /// <summary>
    /// This is labeled as "Is Modifier" because applications should not mistake a temporary or old contact etc.for a current/permanent one. Applications can assume that a contact is current unless it explicitly says that it is temporary or old.
    /// </summary>
    public string Use { get; set; }
    /// <summary>
    /// Extension container element for Use
    /// </summary>
    public Element _Use { get; set; }
    /// <summary>
    /// Additional text data such as phone extension numbers, or notes about use of the contact are sometimes included in the value.
    /// </summary>
    public string Value { get; set; }
    /// <summary>
    /// Extension container element for Value
    /// </summary>
    public Element _Value { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR3.Models.Element)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(System))
      {
        writer.WriteString("system", (string)System!);
      }

      if (_System != null)
      {
        writer.WritePropertyName("_system");
        _System.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Value))
      {
        writer.WriteString("value", (string)Value!);
      }

      if (_Value != null)
      {
        writer.WritePropertyName("_value");
        _Value.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Use))
      {
        writer.WriteString("use", (string)Use!);
      }

      if (_Use != null)
      {
        writer.WritePropertyName("_use");
        _Use.SerializeJson(writer, options);
      }

      if (Rank != null)
      {
        writer.WriteNumber("rank", (uint)Rank!);
      }

      if (Period != null)
      {
        writer.WritePropertyName("period");
        Period.SerializeJson(writer, options);
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
        case "period":
          Period = new fhirCsR3.Models.Period();
          Period.DeserializeJson(ref reader, options);
          break;

        case "rank":
          Rank = reader.GetUInt32();
          break;

        case "system":
          System = reader.GetString();
          break;

        case "_system":
          _System = new fhirCsR3.Models.Element();
          _System.DeserializeJson(ref reader, options);
          break;

        case "use":
          Use = reader.GetString();
          break;

        case "_use":
          _Use = new fhirCsR3.Models.Element();
          _Use.DeserializeJson(ref reader, options);
          break;

        case "value":
          Value = reader.GetString();
          break;

        case "_value":
          _Value = new fhirCsR3.Models.Element();
          _Value.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR3.Models.Element)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the ContactPoint.system field
  /// </summary>
  public static class ContactPointSystemCodes {
    public const string PHONE = "phone";
    public const string FAX = "fax";
    public const string EMAIL = "email";
    public const string PAGER = "pager";
    public const string URL = "url";
    public const string SMS = "sms";
    public const string OTHER = "other";
  }
  /// <summary>
  /// Code Values for the ContactPoint.use field
  /// </summary>
  public static class ContactPointUseCodes {
    public const string HOME = "home";
    public const string WORK = "work";
    public const string TEMP = "temp";
    public const string OLD = "old";
    public const string MOBILE = "mobile";
  }
}
