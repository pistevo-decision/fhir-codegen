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
  /// The anatomical location(s) or region(s) of the specimen, lesion, or body structure.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<BodyStructureIncludedStructure>))]
  public class BodyStructureIncludedStructure : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Code that represents the included structure laterality.
    /// </summary>
    public CodeableConcept Laterality { get; set; }
    /// <summary>
    /// Code that represents the included structure qualifier.
    /// </summary>
    public List<CodeableConcept> Qualifier { get; set; }
    /// <summary>
    /// Code that represents the included structure.
    /// </summary>
    public CodeableConcept Structure { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Structure != null)
      {
        writer.WritePropertyName("structure");
        Structure.SerializeJson(writer, options);
      }

      if (Laterality != null)
      {
        writer.WritePropertyName("laterality");
        Laterality.SerializeJson(writer, options);
      }

      if ((Qualifier != null) && (Qualifier.Count != 0))
      {
        writer.WritePropertyName("qualifier");
        writer.WriteStartArray();

        foreach (CodeableConcept valQualifier in Qualifier)
        {
          valQualifier.SerializeJson(writer, options, true);
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
        case "laterality":
          Laterality = new fhirCsR5.Models.CodeableConcept();
          Laterality.DeserializeJson(ref reader, options);
          break;

        case "qualifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Qualifier = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objQualifier = new fhirCsR5.Models.CodeableConcept();
            objQualifier.DeserializeJson(ref reader, options);
            Qualifier.Add(objQualifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Qualifier.Count == 0)
          {
            Qualifier = null;
          }

          break;

        case "structure":
          Structure = new fhirCsR5.Models.CodeableConcept();
          Structure.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// The anatomical location(s) or region(s) not occupied or represented by the specimen, lesion, or body structure.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<BodyStructureExcludedStructure>))]
  public class BodyStructureExcludedStructure : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Code that represents the excluded structure laterality.
    /// </summary>
    public CodeableConcept Laterality { get; set; }
    /// <summary>
    /// Code that represents the excluded structure qualifier.
    /// </summary>
    public List<CodeableConcept> Qualifier { get; set; }
    /// <summary>
    /// Code that represents the excluded structure.
    /// </summary>
    public CodeableConcept Structure { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR5.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Structure != null)
      {
        writer.WritePropertyName("structure");
        Structure.SerializeJson(writer, options);
      }

      if (Laterality != null)
      {
        writer.WritePropertyName("laterality");
        Laterality.SerializeJson(writer, options);
      }

      if ((Qualifier != null) && (Qualifier.Count != 0))
      {
        writer.WritePropertyName("qualifier");
        writer.WriteStartArray();

        foreach (CodeableConcept valQualifier in Qualifier)
        {
          valQualifier.SerializeJson(writer, options, true);
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
        case "laterality":
          Laterality = new fhirCsR5.Models.CodeableConcept();
          Laterality.DeserializeJson(ref reader, options);
          break;

        case "qualifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Qualifier = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.CodeableConcept objQualifier = new fhirCsR5.Models.CodeableConcept();
            objQualifier.DeserializeJson(ref reader, options);
            Qualifier.Add(objQualifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Qualifier.Count == 0)
          {
            Qualifier = null;
          }

          break;

        case "structure":
          Structure = new fhirCsR5.Models.CodeableConcept();
          Structure.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Record details about an anatomical structure.  This resource may be used when a coded concept does not provide the necessary detail needed for the use case.
  /// </summary>
  [JsonConverter(typeof(fhirCsR5.Serialization.JsonStreamComponentConverter<BodyStructure>))]
  public class BodyStructure : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "BodyStructure";
    /// <summary>
    /// This element is labeled as a modifier because it may be used to mark that the resource was created in error.
    /// </summary>
    public bool? Active { get; set; }
    /// <summary>
    /// This description could include any visual markings used to orientate the viewer e.g. external reference points, special sutures, ink markings.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// The anatomical location(s) or region(s) not occupied or represented by the specimen, lesion, or body structure.
    /// </summary>
    public List<BodyStructureExcludedStructure> ExcludedStructure { get; set; }
    /// <summary>
    /// Identifier for this instance of the anatomical structure.
    /// </summary>
    public List<Identifier> Identifier { get; set; }
    /// <summary>
    /// Image or images used to identify a location.
    /// </summary>
    public List<Attachment> Image { get; set; }
    /// <summary>
    /// The anatomical location(s) or region(s) of the specimen, lesion, or body structure.
    /// </summary>
    public List<BodyStructureIncludedStructure> IncludedStructure { get; set; }
    /// <summary>
    /// The anatomical location or region of the specimen, lesion, or body structure.
    /// </summary>
    public CodeableConcept Location { get; set; }
    /// <summary>
    /// The minimum cardinality of 0 supports the use case of specifying a location without defining a morphology.
    /// </summary>
    public CodeableConcept Morphology { get; set; }
    /// <summary>
    /// The person to which the body site belongs.
    /// </summary>
    public Reference Patient { get; set; }
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


      ((fhirCsR5.Models.DomainResource)this).SerializeJson(writer, options, false);

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

      if (Active != null)
      {
        writer.WriteBoolean("active", (bool)Active!);
      }

      if (Morphology != null)
      {
        writer.WritePropertyName("morphology");
        Morphology.SerializeJson(writer, options);
      }

      if (Location != null)
      {
        writer.WritePropertyName("location");
        Location.SerializeJson(writer, options);
      }

      if ((IncludedStructure != null) && (IncludedStructure.Count != 0))
      {
        writer.WritePropertyName("includedStructure");
        writer.WriteStartArray();

        foreach (BodyStructureIncludedStructure valIncludedStructure in IncludedStructure)
        {
          valIncludedStructure.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((ExcludedStructure != null) && (ExcludedStructure.Count != 0))
      {
        writer.WritePropertyName("excludedStructure");
        writer.WriteStartArray();

        foreach (BodyStructureExcludedStructure valExcludedStructure in ExcludedStructure)
        {
          valExcludedStructure.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (!string.IsNullOrEmpty(Description))
      {
        writer.WriteString("description", (string)Description!);
      }

      if (_Description != null)
      {
        writer.WritePropertyName("_description");
        _Description.SerializeJson(writer, options);
      }

      if ((Image != null) && (Image.Count != 0))
      {
        writer.WritePropertyName("image");
        writer.WriteStartArray();

        foreach (Attachment valImage in Image)
        {
          valImage.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
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
        case "active":
          Active = reader.GetBoolean();
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR5.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "excludedStructure":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          ExcludedStructure = new List<BodyStructureExcludedStructure>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.BodyStructureExcludedStructure objExcludedStructure = new fhirCsR5.Models.BodyStructureExcludedStructure();
            objExcludedStructure.DeserializeJson(ref reader, options);
            ExcludedStructure.Add(objExcludedStructure);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (ExcludedStructure.Count == 0)
          {
            ExcludedStructure = null;
          }

          break;

        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Identifier objIdentifier = new fhirCsR5.Models.Identifier();
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

        case "image":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Image = new List<Attachment>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.Attachment objImage = new fhirCsR5.Models.Attachment();
            objImage.DeserializeJson(ref reader, options);
            Image.Add(objImage);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Image.Count == 0)
          {
            Image = null;
          }

          break;

        case "includedStructure":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          IncludedStructure = new List<BodyStructureIncludedStructure>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR5.Models.BodyStructureIncludedStructure objIncludedStructure = new fhirCsR5.Models.BodyStructureIncludedStructure();
            objIncludedStructure.DeserializeJson(ref reader, options);
            IncludedStructure.Add(objIncludedStructure);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (IncludedStructure.Count == 0)
          {
            IncludedStructure = null;
          }

          break;

        case "location":
          Location = new fhirCsR5.Models.CodeableConcept();
          Location.DeserializeJson(ref reader, options);
          break;

        case "morphology":
          Morphology = new fhirCsR5.Models.CodeableConcept();
          Morphology.DeserializeJson(ref reader, options);
          break;

        case "patient":
          Patient = new fhirCsR5.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR5.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
