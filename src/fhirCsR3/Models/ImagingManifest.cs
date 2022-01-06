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
  /// SOP Instance component identify the instance selected, along with the study and series identities to form the DICOM identity hierarchy.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImagingManifestStudySeriesInstance>))]
  public class ImagingManifestStudySeriesInstance : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// SOP class UID identifies the type of the selected instances, e.g. CT image, Gray scale softcopy presentation state, ECG waveform, etc.
    /// </summary>
    public string SopClass { get; set; }
    /// <summary>
    /// Extension container element for SopClass
    /// </summary>
    public Element _SopClass { get; set; }
    /// <summary>
    /// SOP instance UID identifies the instance.
    /// </summary>
    public string Uid { get; set; }
    /// <summary>
    /// Extension container element for Uid
    /// </summary>
    public Element _Uid { get; set; }
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

      if (!string.IsNullOrEmpty(SopClass))
      {
        writer.WriteString("sopClass", (string)SopClass!);
      }

      if (_SopClass != null)
      {
        writer.WritePropertyName("_sopClass");
        _SopClass.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Uid))
      {
        writer.WriteString("uid", (string)Uid!);
      }

      if (_Uid != null)
      {
        writer.WritePropertyName("_uid");
        _Uid.SerializeJson(writer, options);
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
        case "sopClass":
          SopClass = reader.GetString();
          break;

        case "_sopClass":
          _SopClass = new fhirCsR3.Models.Element();
          _SopClass.DeserializeJson(ref reader, options);
          break;

        case "uid":
          Uid = reader.GetString();
          break;

        case "_uid":
          _Uid = new fhirCsR3.Models.Element();
          _Uid.DeserializeJson(ref reader, options);
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
  /// Series component represents the series level identity and locator information of the DICOM SOP instances in the selection.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImagingManifestStudySeries>))]
  public class ImagingManifestStudySeries : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The network service providing access (e.g., query, view, or retrieval) for this series. See implementation notes for information about using DICOM endpoints. A series-level endpoint, if present, has precedence over a study-level endpoint with the same Endpoint.type.
    /// </summary>
    public List<Reference> Endpoint { get; set; }
    /// <summary>
    /// SOP Instance component identify the instance selected, along with the study and series identities to form the DICOM identity hierarchy.
    /// </summary>
    public List<ImagingManifestStudySeriesInstance> Instance { get; set; }
    /// <summary>
    /// Series instance UID of the SOP instances in the selection.
    /// </summary>
    public string Uid { get; set; }
    /// <summary>
    /// Extension container element for Uid
    /// </summary>
    public Element _Uid { get; set; }
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

      if (!string.IsNullOrEmpty(Uid))
      {
        writer.WriteString("uid", (string)Uid!);
      }

      if (_Uid != null)
      {
        writer.WritePropertyName("_uid");
        _Uid.SerializeJson(writer, options);
      }

      if ((Endpoint != null) && (Endpoint.Count != 0))
      {
        writer.WritePropertyName("endpoint");
        writer.WriteStartArray();

        foreach (Reference valEndpoint in Endpoint)
        {
          valEndpoint.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Instance != null) && (Instance.Count != 0))
      {
        writer.WritePropertyName("instance");
        writer.WriteStartArray();

        foreach (ImagingManifestStudySeriesInstance valInstance in Instance)
        {
          valInstance.SerializeJson(writer, options, true);
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
        case "endpoint":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Endpoint = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objEndpoint = new fhirCsR3.Models.Reference();
            objEndpoint.DeserializeJson(ref reader, options);
            Endpoint.Add(objEndpoint);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Endpoint.Count == 0)
          {
            Endpoint = null;
          }

          break;

        case "instance":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Instance = new List<ImagingManifestStudySeriesInstance>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ImagingManifestStudySeriesInstance objInstance = new fhirCsR3.Models.ImagingManifestStudySeriesInstance();
            objInstance.DeserializeJson(ref reader, options);
            Instance.Add(objInstance);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Instance.Count == 0)
          {
            Instance = null;
          }

          break;

        case "uid":
          Uid = reader.GetString();
          break;

        case "_uid":
          _Uid = new fhirCsR3.Models.Element();
          _Uid.DeserializeJson(ref reader, options);
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
  /// Study component represents the study level identity and locator information of the DICOM SOP instances in the selection. It is the top level identity of the hierarchical identification of the instances.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImagingManifestStudy>))]
  public class ImagingManifestStudy : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// The network service providing access (e.g., query, view, or retrieval) for the study. See implementation notes for information about using DICOM endpoints. A study-level endpoint applies to each series in the study, unless overridden by a series-level endpoint with the same Endpoint.type.
    /// </summary>
    public List<Reference> Endpoint { get; set; }
    /// <summary>
    /// Reference to the Imaging Study in FHIR form.
    /// </summary>
    public Reference ImagingStudy { get; set; }
    /// <summary>
    /// Series component represents the series level identity and locator information of the DICOM SOP instances in the selection.
    /// </summary>
    public List<ImagingManifestStudySeries> Series { get; set; }
    /// <summary>
    /// Study instance UID of the SOP instances in the selection.
    /// </summary>
    public string Uid { get; set; }
    /// <summary>
    /// Extension container element for Uid
    /// </summary>
    public Element _Uid { get; set; }
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

      if (!string.IsNullOrEmpty(Uid))
      {
        writer.WriteString("uid", (string)Uid!);
      }

      if (_Uid != null)
      {
        writer.WritePropertyName("_uid");
        _Uid.SerializeJson(writer, options);
      }

      if (ImagingStudy != null)
      {
        writer.WritePropertyName("imagingStudy");
        ImagingStudy.SerializeJson(writer, options);
      }

      if ((Endpoint != null) && (Endpoint.Count != 0))
      {
        writer.WritePropertyName("endpoint");
        writer.WriteStartArray();

        foreach (Reference valEndpoint in Endpoint)
        {
          valEndpoint.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Series != null) && (Series.Count != 0))
      {
        writer.WritePropertyName("series");
        writer.WriteStartArray();

        foreach (ImagingManifestStudySeries valSeries in Series)
        {
          valSeries.SerializeJson(writer, options, true);
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
        case "endpoint":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Endpoint = new List<Reference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.Reference objEndpoint = new fhirCsR3.Models.Reference();
            objEndpoint.DeserializeJson(ref reader, options);
            Endpoint.Add(objEndpoint);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Endpoint.Count == 0)
          {
            Endpoint = null;
          }

          break;

        case "imagingStudy":
          ImagingStudy = new fhirCsR3.Models.Reference();
          ImagingStudy.DeserializeJson(ref reader, options);
          break;

        case "series":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Series = new List<ImagingManifestStudySeries>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ImagingManifestStudySeries objSeries = new fhirCsR3.Models.ImagingManifestStudySeries();
            objSeries.DeserializeJson(ref reader, options);
            Series.Add(objSeries);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Series.Count == 0)
          {
            Series = null;
          }

          break;

        case "uid":
          Uid = reader.GetString();
          break;

        case "_uid":
          _Uid = new fhirCsR3.Models.Element();
          _Uid.DeserializeJson(ref reader, options);
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
  /// A text description of the DICOM SOP instances selected in the ImagingManifest; or the reason for, or significance of, the selection.
  /// </summary>
  [JsonConverter(typeof(fhirCsR3.Serialization.JsonStreamComponentConverter<ImagingManifest>))]
  public class ImagingManifest : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "ImagingManifest";
    /// <summary>
    /// Author of ImagingManifest. It can be a human author or a device which made the decision of the SOP instances selected. For example, a radiologist selected a set of imaging SOP instances to attach in a diagnostic report, and a CAD application may author a selection to describe SOP instances it used to generate a detection conclusion.
    /// </summary>
    public Reference Author { get; set; }
    /// <summary>
    /// Date and time when the selection of the referenced instances were made. It is (typically) different from the creation date of the selection resource, and from dates associated with the referenced instances (e.g. capture time of the referenced image).
    /// </summary>
    public string AuthoringTime { get; set; }
    /// <summary>
    /// Extension container element for AuthoringTime
    /// </summary>
    public Element _AuthoringTime { get; set; }
    /// <summary>
    /// Free text narrative description of the ImagingManifest.
    /// </summary>
    public string Description { get; set; }
    /// <summary>
    /// Extension container element for Description
    /// </summary>
    public Element _Description { get; set; }
    /// <summary>
    /// Unique identifier of the DICOM Key Object Selection (KOS) that this resource represents.
    /// </summary>
    public Identifier Identifier { get; set; }
    /// <summary>
    /// SOP instances selected in the ImagingManifest can be from different studies, but must be of the same patient.
    /// </summary>
    public Reference Patient { get; set; }
    /// <summary>
    /// Study component represents the study level identity and locator information of the DICOM SOP instances in the selection. It is the top level identity of the hierarchical identification of the instances.
    /// </summary>
    public List<ImagingManifestStudy> Study { get; set; }
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

      if (Identifier != null)
      {
        writer.WritePropertyName("identifier");
        Identifier.SerializeJson(writer, options);
      }

      if (Patient != null)
      {
        writer.WritePropertyName("patient");
        Patient.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(AuthoringTime))
      {
        writer.WriteString("authoringTime", (string)AuthoringTime!);
      }

      if (_AuthoringTime != null)
      {
        writer.WritePropertyName("_authoringTime");
        _AuthoringTime.SerializeJson(writer, options);
      }

      if (Author != null)
      {
        writer.WritePropertyName("author");
        Author.SerializeJson(writer, options);
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

      if ((Study != null) && (Study.Count != 0))
      {
        writer.WritePropertyName("study");
        writer.WriteStartArray();

        foreach (ImagingManifestStudy valStudy in Study)
        {
          valStudy.SerializeJson(writer, options, true);
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
        case "author":
          Author = new fhirCsR3.Models.Reference();
          Author.DeserializeJson(ref reader, options);
          break;

        case "authoringTime":
          AuthoringTime = reader.GetString();
          break;

        case "_authoringTime":
          _AuthoringTime = new fhirCsR3.Models.Element();
          _AuthoringTime.DeserializeJson(ref reader, options);
          break;

        case "description":
          Description = reader.GetString();
          break;

        case "_description":
          _Description = new fhirCsR3.Models.Element();
          _Description.DeserializeJson(ref reader, options);
          break;

        case "identifier":
          Identifier = new fhirCsR3.Models.Identifier();
          Identifier.DeserializeJson(ref reader, options);
          break;

        case "patient":
          Patient = new fhirCsR3.Models.Reference();
          Patient.DeserializeJson(ref reader, options);
          break;

        case "study":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Study = new List<ImagingManifestStudy>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR3.Models.ImagingManifestStudy objStudy = new fhirCsR3.Models.ImagingManifestStudy();
            objStudy.DeserializeJson(ref reader, options);
            Study.Add(objStudy);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Study.Count == 0)
          {
            Study = null;
          }

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
}
