// <auto-generated />
// Built from: hl7.fhir.r5.core version: 5.0.0-snapshot1
  // Option: "NAMESPACE" = "fhirCsR5"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR5.Serialization;

namespace fhirCsR5.Models
{
  /// <summary>
  /// A description of a triggering event. Triggering events can be named events, data events, or periodic, as determined by the type element.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<TriggerDefinition>))]
  public class TriggerDefinition : DataType,  IFhirJsonSerializable {
    /// <summary>
    /// This element can be only be specified for data type triggers and provides additional semantics for the trigger. The context available within the condition is based on the type of data event. For all events, the current resource will be available as context. In addition, for modification events, the previous resource will also be available. The expression may be inlined, or may be a simple absolute URI, which is a reference to a named expression within a logic library referenced by a library element or extension within the containing resource. If the expression is a FHIR Path expression, it evaluates in the context of a resource of one of the type identified in the data requirement, and may also refer to the variable %previous for delta comparisons on events of type data-changed, data-modified, and data-deleted which will always have the same type.
    /// </summary>
    public Expression Condition { get; set; }
    /// <summary>
    /// This element shall be present for any data type trigger.
    /// </summary>
    public List<DataRequirement> Data { get; set; }
    /// <summary>
    /// An event name can be provided for all event types, but is required for named events. If a name is provided for a type other than named events, it is considered to be a shorthand for the semantics described by the formal description of the event.
    /// </summary>
    public string Name { get; set; }
    /// <summary>
    /// Extension container element for Name
    /// </summary>
    public Element _Name { get; set; }
    /// <summary>
    /// The timing of the event (if this is a periodic trigger).
    /// </summary>
    public Timing TimingTiming { get; set; }
    /// <summary>
    /// The timing of the event (if this is a periodic trigger).
    /// </summary>
    public Reference TimingReference { get; set; }
    /// <summary>
    /// The timing of the event (if this is a periodic trigger).
    /// </summary>
    public string TimingDate { get; set; }
    /// <summary>
    /// Extension container element for TimingDate
    /// </summary>
    public Element _TimingDate { get; set; }
    /// <summary>
    /// The timing of the event (if this is a periodic trigger).
    /// </summary>
    public string TimingDateTime { get; set; }
    /// <summary>
    /// Extension container element for TimingDateTime
    /// </summary>
    public Element _TimingDateTime { get; set; }
    /// <summary>
    /// The type of triggering event.
    /// </summary>
    public string Type { get; set; }
    /// <summary>
    /// Extension container element for Type
    /// </summary>
    public Element _Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.DataType)this).SerializeJson(writer, options, false);

      if (!string.IsNullOrEmpty(Type))
      {
        writer.WriteString("type", (string)Type!);
      }

      if (_Type != null)
      {
        writer.WritePropertyName("_type");
        _Type.SerializeJson(writer, options);
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

      if (TimingTiming != null)
      {
        writer.WritePropertyName("timingTiming");
        TimingTiming.SerializeJson(writer, options);
      }

      if (TimingReference != null)
      {
        writer.WritePropertyName("timingReference");
        TimingReference.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(TimingDate))
      {
        writer.WriteString("timingDate", (string)TimingDate!);
      }

      if (_TimingDate != null)
      {
        writer.WritePropertyName("_timingDate");
        _TimingDate.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(TimingDateTime))
      {
        writer.WriteString("timingDateTime", (string)TimingDateTime!);
      }

      if (_TimingDateTime != null)
      {
        writer.WritePropertyName("_timingDateTime");
        _TimingDateTime.SerializeJson(writer, options);
      }

      if ((Data != null) && (Data.Count != 0))
      {
        writer.WritePropertyName("data");
        writer.WriteStartArray();

        foreach (DataRequirement valData in Data)
        {
          valData.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Condition != null)
      {
        writer.WritePropertyName("condition");
        Condition.SerializeJson(writer, options);
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
        case "condition":
          Condition = new fhirCsR5.Models.Expression();
          Condition.DeserializeJson(ref reader, options);
          break;

        case "data":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Data = new List<DataRequirement>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.DataRequirement objData = new fhirCsR5.Models.DataRequirement();
            objData.DeserializeJson(ref reader, options);
            Data.Add(objData);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Data.Count == 0)
          {
            Data = null;
          }

          break;

        case "name":
          Name = reader.GetString();
          break;

        case "_name":
          _Name = new fhirCsR5.Models.Element();
          _Name.DeserializeJson(ref reader, options);
          break;

        case "timingTiming":
          TimingTiming = new fhirCsR5.Models.Timing();
          TimingTiming.DeserializeJson(ref reader, options);
          break;

        case "timingReference":
          TimingReference = new fhirCsR5.Models.Reference();
          TimingReference.DeserializeJson(ref reader, options);
          break;

        case "timingDate":
          TimingDate = reader.GetString();
          break;

        case "_timingDate":
          _TimingDate = new fhirCsR5.Models.Element();
          _TimingDate.DeserializeJson(ref reader, options);
          break;

        case "timingDateTime":
          TimingDateTime = reader.GetString();
          break;

        case "_timingDateTime":
          _TimingDateTime = new fhirCsR5.Models.Element();
          _TimingDateTime.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = reader.GetString();
          break;

        case "_type":
          _Type = new fhirCsR5.Models.Element();
          _Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DataType)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Code Values for the TriggerDefinition.type field
  /// </summary>
  public static class TriggerDefinitionTypeCodes {
    public const string NAMED_EVENT = "named-event";
    public const string PERIODIC = "periodic";
    public const string DATA_CHANGED = "data-changed";
    public const string DATA_ADDED = "data-added";
    public const string DATA_MODIFIED = "data-modified";
    public const string DATA_REMOVED = "data-removed";
    public const string DATA_ACCESSED = "data-accessed";
    public const string DATA_ACCESS_ENDED = "data-access-ended";
  }
}
