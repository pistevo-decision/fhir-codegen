// <auto-generated />
// Built from: hl7.fhir.r4.core version: 4.0.1
  // Option: "NAMESPACE" = "fhirCsR4"

using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Serialization;
using fhirCsR4.Serialization;

namespace fhirCsR4.Models
{
  /// <summary>
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymerMonomerSetStartingMaterial>))]
  public class SubstancePolymerMonomerSetStartingMaterial : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Todo.
    /// </summary>
    public SubstanceAmount Amount { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public bool? IsDefining { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept Material { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Material != null)
      {
        writer.WritePropertyName("material");
        Material.SerializeJson(writer, options);
      }

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (IsDefining != null)
      {
        writer.WriteBoolean("isDefining", (bool)IsDefining!);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
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
        case "amount":
          Amount = new fhirCsR4.Models.SubstanceAmount();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "isDefining":
          IsDefining = reader.GetBoolean();
          break;

        case "material":
          Material = new fhirCsR4.Models.CodeableConcept();
          Material.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR4.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymerMonomerSet>))]
  public class SubstancePolymerMonomerSet : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept RatioType { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<SubstancePolymerMonomerSetStartingMaterial> StartingMaterial { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (RatioType != null)
      {
        writer.WritePropertyName("ratioType");
        RatioType.SerializeJson(writer, options);
      }

      if ((StartingMaterial != null) && (StartingMaterial.Count != 0))
      {
        writer.WritePropertyName("startingMaterial");
        writer.WriteStartArray();

        foreach (SubstancePolymerMonomerSetStartingMaterial valStartingMaterial in StartingMaterial)
        {
          valStartingMaterial.SerializeJson(writer, options, true);
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
        case "ratioType":
          RatioType = new fhirCsR4.Models.CodeableConcept();
          RatioType.DeserializeJson(ref reader, options);
          break;

        case "startingMaterial":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          StartingMaterial = new List<SubstancePolymerMonomerSetStartingMaterial>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstancePolymerMonomerSetStartingMaterial objStartingMaterial = new fhirCsR4.Models.SubstancePolymerMonomerSetStartingMaterial();
            objStartingMaterial.DeserializeJson(ref reader, options);
            StartingMaterial.Add(objStartingMaterial);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (StartingMaterial.Count == 0)
          {
            StartingMaterial = null;
          }

          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation>))]
  public class SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Todo.
    /// </summary>
    public SubstanceAmount Amount { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept Degree { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Degree != null)
      {
        writer.WritePropertyName("degree");
        Degree.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
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
        case "amount":
          Amount = new fhirCsR4.Models.SubstanceAmount();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "degree":
          Degree = new fhirCsR4.Models.CodeableConcept();
          Degree.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymerRepeatRepeatUnitStructuralRepresentation>))]
  public class SubstancePolymerRepeatRepeatUnitStructuralRepresentation : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Todo.
    /// </summary>
    public Attachment Attachment { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public string Representation { get; set; }
    /// <summary>
    /// Extension container element for Representation
    /// </summary>
    public Element _Representation { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept Type { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (Type != null)
      {
        writer.WritePropertyName("type");
        Type.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(Representation))
      {
        writer.WriteString("representation", (string)Representation!);
      }

      if (_Representation != null)
      {
        writer.WritePropertyName("_representation");
        _Representation.SerializeJson(writer, options);
      }

      if (Attachment != null)
      {
        writer.WritePropertyName("attachment");
        Attachment.SerializeJson(writer, options);
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
        case "attachment":
          Attachment = new fhirCsR4.Models.Attachment();
          Attachment.DeserializeJson(ref reader, options);
          break;

        case "representation":
          Representation = reader.GetString();
          break;

        case "_representation":
          _Representation = new fhirCsR4.Models.Element();
          _Representation.DeserializeJson(ref reader, options);
          break;

        case "type":
          Type = new fhirCsR4.Models.CodeableConcept();
          Type.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymerRepeatRepeatUnit>))]
  public class SubstancePolymerRepeatRepeatUnit : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Todo.
    /// </summary>
    public SubstanceAmount Amount { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation> DegreeOfPolymerisation { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept OrientationOfPolymerisation { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public string RepeatUnit { get; set; }
    /// <summary>
    /// Extension container element for RepeatUnit
    /// </summary>
    public Element _RepeatUnit { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<SubstancePolymerRepeatRepeatUnitStructuralRepresentation> StructuralRepresentation { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (OrientationOfPolymerisation != null)
      {
        writer.WritePropertyName("orientationOfPolymerisation");
        OrientationOfPolymerisation.SerializeJson(writer, options);
      }

      if (!string.IsNullOrEmpty(RepeatUnit))
      {
        writer.WriteString("repeatUnit", (string)RepeatUnit!);
      }

      if (_RepeatUnit != null)
      {
        writer.WritePropertyName("_repeatUnit");
        _RepeatUnit.SerializeJson(writer, options);
      }

      if (Amount != null)
      {
        writer.WritePropertyName("amount");
        Amount.SerializeJson(writer, options);
      }

      if ((DegreeOfPolymerisation != null) && (DegreeOfPolymerisation.Count != 0))
      {
        writer.WritePropertyName("degreeOfPolymerisation");
        writer.WriteStartArray();

        foreach (SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation valDegreeOfPolymerisation in DegreeOfPolymerisation)
        {
          valDegreeOfPolymerisation.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((StructuralRepresentation != null) && (StructuralRepresentation.Count != 0))
      {
        writer.WritePropertyName("structuralRepresentation");
        writer.WriteStartArray();

        foreach (SubstancePolymerRepeatRepeatUnitStructuralRepresentation valStructuralRepresentation in StructuralRepresentation)
        {
          valStructuralRepresentation.SerializeJson(writer, options, true);
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
        case "amount":
          Amount = new fhirCsR4.Models.SubstanceAmount();
          Amount.DeserializeJson(ref reader, options);
          break;

        case "degreeOfPolymerisation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          DegreeOfPolymerisation = new List<SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation objDegreeOfPolymerisation = new fhirCsR4.Models.SubstancePolymerRepeatRepeatUnitDegreeOfPolymerisation();
            objDegreeOfPolymerisation.DeserializeJson(ref reader, options);
            DegreeOfPolymerisation.Add(objDegreeOfPolymerisation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (DegreeOfPolymerisation.Count == 0)
          {
            DegreeOfPolymerisation = null;
          }

          break;

        case "orientationOfPolymerisation":
          OrientationOfPolymerisation = new fhirCsR4.Models.CodeableConcept();
          OrientationOfPolymerisation.DeserializeJson(ref reader, options);
          break;

        case "repeatUnit":
          RepeatUnit = reader.GetString();
          break;

        case "_repeatUnit":
          _RepeatUnit = new fhirCsR4.Models.Element();
          _RepeatUnit.DeserializeJson(ref reader, options);
          break;

        case "structuralRepresentation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          StructuralRepresentation = new List<SubstancePolymerRepeatRepeatUnitStructuralRepresentation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstancePolymerRepeatRepeatUnitStructuralRepresentation objStructuralRepresentation = new fhirCsR4.Models.SubstancePolymerRepeatRepeatUnitStructuralRepresentation();
            objStructuralRepresentation.DeserializeJson(ref reader, options);
            StructuralRepresentation.Add(objStructuralRepresentation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (StructuralRepresentation.Count == 0)
          {
            StructuralRepresentation = null;
          }

          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymerRepeat>))]
  public class SubstancePolymerRepeat : BackboneElement,  IFhirJsonSerializable {
    /// <summary>
    /// Todo.
    /// </summary>
    public string AverageMolecularFormula { get; set; }
    /// <summary>
    /// Extension container element for AverageMolecularFormula
    /// </summary>
    public Element _AverageMolecularFormula { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public int? NumberOfUnits { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<SubstancePolymerRepeatRepeatUnit> RepeatUnit { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept RepeatUnitAmountType { get; set; }
    /// <summary>
    /// Serialize to a JSON object
    /// </summary>
    public new void SerializeJson(Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject)
      {
        writer.WriteStartObject();
      }
      ((fhirCsR4.Models.BackboneElement)this).SerializeJson(writer, options, false);

      if (NumberOfUnits != null)
      {
        writer.WriteNumber("numberOfUnits", (int)NumberOfUnits!);
      }

      if (!string.IsNullOrEmpty(AverageMolecularFormula))
      {
        writer.WriteString("averageMolecularFormula", (string)AverageMolecularFormula!);
      }

      if (_AverageMolecularFormula != null)
      {
        writer.WritePropertyName("_averageMolecularFormula");
        _AverageMolecularFormula.SerializeJson(writer, options);
      }

      if (RepeatUnitAmountType != null)
      {
        writer.WritePropertyName("repeatUnitAmountType");
        RepeatUnitAmountType.SerializeJson(writer, options);
      }

      if ((RepeatUnit != null) && (RepeatUnit.Count != 0))
      {
        writer.WritePropertyName("repeatUnit");
        writer.WriteStartArray();

        foreach (SubstancePolymerRepeatRepeatUnit valRepeatUnit in RepeatUnit)
        {
          valRepeatUnit.SerializeJson(writer, options, true);
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
        case "averageMolecularFormula":
          AverageMolecularFormula = reader.GetString();
          break;

        case "_averageMolecularFormula":
          _AverageMolecularFormula = new fhirCsR4.Models.Element();
          _AverageMolecularFormula.DeserializeJson(ref reader, options);
          break;

        case "numberOfUnits":
          NumberOfUnits = reader.GetInt32();
          break;

        case "repeatUnit":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          RepeatUnit = new List<SubstancePolymerRepeatRepeatUnit>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstancePolymerRepeatRepeatUnit objRepeatUnit = new fhirCsR4.Models.SubstancePolymerRepeatRepeatUnit();
            objRepeatUnit.DeserializeJson(ref reader, options);
            RepeatUnit.Add(objRepeatUnit);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (RepeatUnit.Count == 0)
          {
            RepeatUnit = null;
          }

          break;

        case "repeatUnitAmountType":
          RepeatUnitAmountType = new fhirCsR4.Models.CodeableConcept();
          RepeatUnitAmountType.DeserializeJson(ref reader, options);
          break;

        default:
          ((fhirCsR4.Models.BackboneElement)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
  /// Todo.
  /// </summary>
  [JsonConverter(typeof(fhirCsR4.Serialization.JsonStreamComponentConverter<SubstancePolymer>))]
  public class SubstancePolymer : DomainResource,  IFhirJsonSerializable {
    /// <summary>
    /// Resource Type Name
    /// </summary>
    public override string ResourceType => "SubstancePolymer";
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept Class { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<CodeableConcept> CopolymerConnectivity { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public CodeableConcept Geometry { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<string> Modification { get; set; }
    /// <summary>
    /// Extension container element for Modification
    /// </summary>
    public List<Element> _Modification { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<SubstancePolymerMonomerSet> MonomerSet { get; set; }
    /// <summary>
    /// Todo.
    /// </summary>
    public List<SubstancePolymerRepeat> Repeat { get; set; }
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


      ((fhirCsR4.Models.DomainResource)this).SerializeJson(writer, options, false);

      if (Class != null)
      {
        writer.WritePropertyName("class");
        Class.SerializeJson(writer, options);
      }

      if (Geometry != null)
      {
        writer.WritePropertyName("geometry");
        Geometry.SerializeJson(writer, options);
      }

      if ((CopolymerConnectivity != null) && (CopolymerConnectivity.Count != 0))
      {
        writer.WritePropertyName("copolymerConnectivity");
        writer.WriteStartArray();

        foreach (CodeableConcept valCopolymerConnectivity in CopolymerConnectivity)
        {
          valCopolymerConnectivity.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Modification != null) && (Modification.Count != 0))
      {
        writer.WritePropertyName("modification");
        writer.WriteStartArray();

        foreach (string valModification in Modification)
        {
          writer.WriteStringValue(valModification);
        }

        writer.WriteEndArray();
      }

      if ((_Modification != null) && (_Modification.Count != 0))
      {
        writer.WritePropertyName("_modification");
        writer.WriteStartArray();

        foreach (Element val_Modification in _Modification)
        {
          val_Modification.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((MonomerSet != null) && (MonomerSet.Count != 0))
      {
        writer.WritePropertyName("monomerSet");
        writer.WriteStartArray();

        foreach (SubstancePolymerMonomerSet valMonomerSet in MonomerSet)
        {
          valMonomerSet.SerializeJson(writer, options, true);
        }

        writer.WriteEndArray();
      }

      if ((Repeat != null) && (Repeat.Count != 0))
      {
        writer.WritePropertyName("repeat");
        writer.WriteStartArray();

        foreach (SubstancePolymerRepeat valRepeat in Repeat)
        {
          valRepeat.SerializeJson(writer, options, true);
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
        case "class":
          Class = new fhirCsR4.Models.CodeableConcept();
          Class.DeserializeJson(ref reader, options);
          break;

        case "copolymerConnectivity":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          CopolymerConnectivity = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.CodeableConcept objCopolymerConnectivity = new fhirCsR4.Models.CodeableConcept();
            objCopolymerConnectivity.DeserializeJson(ref reader, options);
            CopolymerConnectivity.Add(objCopolymerConnectivity);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (CopolymerConnectivity.Count == 0)
          {
            CopolymerConnectivity = null;
          }

          break;

        case "geometry":
          Geometry = new fhirCsR4.Models.CodeableConcept();
          Geometry.DeserializeJson(ref reader, options);
          break;

        case "modification":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Modification = new List<string>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Modification.Add(reader.GetString());

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Modification.Count == 0)
          {
            Modification = null;
          }

          break;

        case "_modification":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          _Modification = new List<Element>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.Element obj_Modification = new fhirCsR4.Models.Element();
            obj_Modification.DeserializeJson(ref reader, options);
            _Modification.Add(obj_Modification);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (_Modification.Count == 0)
          {
            _Modification = null;
          }

          break;

        case "monomerSet":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          MonomerSet = new List<SubstancePolymerMonomerSet>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstancePolymerMonomerSet objMonomerSet = new fhirCsR4.Models.SubstancePolymerMonomerSet();
            objMonomerSet.DeserializeJson(ref reader, options);
            MonomerSet.Add(objMonomerSet);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (MonomerSet.Count == 0)
          {
            MonomerSet = null;
          }

          break;

        case "repeat":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          Repeat = new List<SubstancePolymerRepeat>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            fhirCsR4.Models.SubstancePolymerRepeat objRepeat = new fhirCsR4.Models.SubstancePolymerRepeat();
            objRepeat.DeserializeJson(ref reader, options);
            Repeat.Add(objRepeat);

            if (!reader.Read())
            {
              throw new JsonException();
            }
          }

          if (Repeat.Count == 0)
          {
            Repeat = null;
          }

          break;

        default:
          ((fhirCsR4.Models.DomainResource)this).DeserializeJsonProperty(ref reader, options, propertyName);
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
