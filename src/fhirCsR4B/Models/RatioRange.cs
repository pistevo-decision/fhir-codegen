// <auto-generated />
// Built from: hl7.fhir.r4b.core version: 4.3.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR4B"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4B.Serialization;

namespace fhirCsR4B.Models
{
  /// <summary>
  /// A range of ratios expressed as a low and high numerator and a denominator.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4B.Serialization.JsonStreamComponentConverter<RatioRange>))]
  public class RatioRange : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// The value of the denominator.
    /// </summary>
    public Quantity Denominator { get; set; }
    /// <summary>
    /// If the high element is missing, the high boundary is not known.
    /// </summary>
    public Quantity HighNumerator { get; set; }
    /// <summary>
    /// If the low element is missing, the low boundary is not known.
    /// </summary>
    public Quantity LowNumerator { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4B.Models.DataType)this).SerializeJson(writer, options, false);

      if (LowNumerator != null)
      {
        writer.WritePropertyName("lowNumerator");
        LowNumerator.SerializeJson(writer, options);
      }

      if (HighNumerator != null)
      {
        writer.WritePropertyName("highNumerator");
        HighNumerator.SerializeJson(writer, options);
      }

      if (Denominator != null)
      {
        writer.WritePropertyName("denominator");
        Denominator.SerializeJson(writer, options);
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
        case "denominator":
          Denominator = new fhirCsR4B.Models.Quantity();
          Denominator.DeserializeJson(ref reader, options);
          break;

        case "highNumerator":
          HighNumerator = new fhirCsR4B.Models.Quantity();
          HighNumerator.DeserializeJson(ref reader, options);
          break;

        case "lowNumerator":
          LowNumerator = new fhirCsR4B.Models.Quantity();
          LowNumerator.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4B.Models.DataType)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
}