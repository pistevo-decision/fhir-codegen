// <auto-generated/>
// Contents of: hl7.fhir.r4.core version: 4.0.1

using System;
using System.Buffers;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Text.Json.Serialization;
using Hl7.Fhir.Model;
using Hl7.Fhir.Model.JsonExtensions;
using Hl7.Fhir.Serialization;

/*
  Copyright (c) 2011+, HL7, Inc.
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without modification, 
  are permitted provided that the following conditions are met:
  
   * Redistributions of source code must retain the above copyright notice, this 
     list of conditions and the following disclaimer.
   * Redistributions in binary form must reproduce the above copyright notice, 
     this list of conditions and the following disclaimer in the documentation 
     and/or other materials provided with the distribution.
   * Neither the name of HL7 nor the names of its contributors may be used to 
     endorse or promote products derived from this software without specific 
     prior written permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND 
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. 
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, 
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT 
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR 
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
  POSSIBILITY OF SUCH DAMAGE.
  
*/

namespace Hl7.Fhir.Model.JsonExtensions
{
  /// <summary>
  /// JSON Serialization Extensions for SpecimenDefinition
  /// </summary>
  public static class SpecimenDefinitionJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR SpecimenDefinition into JSON
    /// </summary>
    public static void SerializeJson(this SpecimenDefinition current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","SpecimenDefinition");
      // Complex: SpecimenDefinition, Export: SpecimenDefinition, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if (current.Identifier != null)
      {
        writer.WritePropertyName("identifier");
        current.Identifier.SerializeJson(writer, options);
      }

      if (current.TypeCollected != null)
      {
        writer.WritePropertyName("typeCollected");
        current.TypeCollected.SerializeJson(writer, options);
      }

      if ((current.PatientPreparation != null) && (current.PatientPreparation.Count != 0))
      {
        writer.WritePropertyName("patientPreparation");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.PatientPreparation)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.TimeAspectElement != null)
      {
        if (!string.IsNullOrEmpty(current.TimeAspectElement.Value))
        {
          writer.WriteString("timeAspect",current.TimeAspectElement.Value);
        }
        if (current.TimeAspectElement.HasExtensions() || (!string.IsNullOrEmpty(current.TimeAspectElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_timeAspect",false,current.TimeAspectElement.Extension,current.TimeAspectElement.ElementId);
        }
      }

      if ((current.Collection != null) && (current.Collection.Count != 0))
      {
        writer.WritePropertyName("collection");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Collection)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.TypeTested != null) && (current.TypeTested.Count != 0))
      {
        writer.WritePropertyName("typeTested");
        writer.WriteStartArray();
        foreach (SpecimenDefinition.TypeTestedComponent val in current.TypeTested)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition
    /// </summary>
    public static void DeserializeJson(this SpecimenDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition
    /// </summary>
    public static void DeserializeJsonProperty(this SpecimenDefinition current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          current.Identifier = new Hl7.Fhir.Model.Identifier();
          ((Hl7.Fhir.Model.Identifier)current.Identifier).DeserializeJson(ref reader, options);
          break;

        case "typeCollected":
          current.TypeCollected = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.TypeCollected).DeserializeJson(ref reader, options);
          break;

        case "patientPreparation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.PatientPreparation = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_PatientPreparation = new Hl7.Fhir.Model.CodeableConcept();
            v_PatientPreparation.DeserializeJson(ref reader, options);
            current.PatientPreparation.Add(v_PatientPreparation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.PatientPreparation.Count == 0)
          {
            current.PatientPreparation = null;
          }
          break;

        case "timeAspect":
          current.TimeAspectElement = new FhirString(reader.GetString());
          break;

        case "_timeAspect":
          ((Hl7.Fhir.Model.Element)current.TimeAspectElement).DeserializeJson(ref reader, options);
          break;

        case "collection":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Collection = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Collection = new Hl7.Fhir.Model.CodeableConcept();
            v_Collection.DeserializeJson(ref reader, options);
            current.Collection.Add(v_Collection);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Collection.Count == 0)
          {
            current.Collection = null;
          }
          break;

        case "typeTested":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.TypeTested = new List<SpecimenDefinition.TypeTestedComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.SpecimenDefinition.TypeTestedComponent v_TypeTested = new Hl7.Fhir.Model.SpecimenDefinition.TypeTestedComponent();
            v_TypeTested.DeserializeJson(ref reader, options);
            current.TypeTested.Add(v_TypeTested);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.TypeTested.Count == 0)
          {
            current.TypeTested = null;
          }
          break;

        // Complex: SpecimenDefinition, Export: SpecimenDefinition, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR SpecimenDefinition#TypeTested into JSON
    /// </summary>
    public static void SerializeJson(this SpecimenDefinition.TypeTestedComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: SpecimenDefinition#TypeTested, Export: TypeTestedComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.IsDerivedElement != null)
      {
        if (current.IsDerivedElement.Value != null)
        {
          writer.WriteBoolean("isDerived",(bool)current.IsDerivedElement.Value);
        }
        if (current.IsDerivedElement.HasExtensions() || (!string.IsNullOrEmpty(current.IsDerivedElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_isDerived",false,current.IsDerivedElement.Extension,current.IsDerivedElement.ElementId);
        }
      }

      if (current.Type != null)
      {
        writer.WritePropertyName("type");
        current.Type.SerializeJson(writer, options);
      }

      writer.WriteString("preference",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.PreferenceElement.Value));

      if (current.Container != null)
      {
        writer.WritePropertyName("container");
        current.Container.SerializeJson(writer, options);
      }

      if (current.RequirementElement != null)
      {
        if (!string.IsNullOrEmpty(current.RequirementElement.Value))
        {
          writer.WriteString("requirement",current.RequirementElement.Value);
        }
        if (current.RequirementElement.HasExtensions() || (!string.IsNullOrEmpty(current.RequirementElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_requirement",false,current.RequirementElement.Extension,current.RequirementElement.ElementId);
        }
      }

      if (current.RetentionTime != null)
      {
        writer.WritePropertyName("retentionTime");
        current.RetentionTime.SerializeJson(writer, options);
      }

      if ((current.RejectionCriterion != null) && (current.RejectionCriterion.Count != 0))
      {
        writer.WritePropertyName("rejectionCriterion");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.RejectionCriterion)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Handling != null) && (current.Handling.Count != 0))
      {
        writer.WritePropertyName("handling");
        writer.WriteStartArray();
        foreach (SpecimenDefinition.HandlingComponent val in current.Handling)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#TypeTested
    /// </summary>
    public static void DeserializeJson(this SpecimenDefinition.TypeTestedComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#TypeTested
    /// </summary>
    public static void DeserializeJsonProperty(this SpecimenDefinition.TypeTestedComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "isDerived":
          current.IsDerivedElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "_isDerived":
          ((Hl7.Fhir.Model.Element)current.IsDerivedElement).DeserializeJson(ref reader, options);
          break;

        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "preference":
          current.PreferenceElement =new Code<Hl7.Fhir.Model.SpecimenDefinition.SpecimenContainedPreference>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.SpecimenDefinition.SpecimenContainedPreference>(reader.GetString()));
          break;

        case "_preference":
          ((Hl7.Fhir.Model.Element)current.PreferenceElement).DeserializeJson(ref reader, options);
          break;

        case "container":
          current.Container = new Hl7.Fhir.Model.SpecimenDefinition.ContainerComponent();
          ((Hl7.Fhir.Model.SpecimenDefinition.ContainerComponent)current.Container).DeserializeJson(ref reader, options);
          break;

        case "requirement":
          current.RequirementElement = new FhirString(reader.GetString());
          break;

        case "_requirement":
          ((Hl7.Fhir.Model.Element)current.RequirementElement).DeserializeJson(ref reader, options);
          break;

        case "retentionTime":
          current.RetentionTime = new Hl7.Fhir.Model.Duration();
          ((Hl7.Fhir.Model.Duration)current.RetentionTime).DeserializeJson(ref reader, options);
          break;

        case "rejectionCriterion":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.RejectionCriterion = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_RejectionCriterion = new Hl7.Fhir.Model.CodeableConcept();
            v_RejectionCriterion.DeserializeJson(ref reader, options);
            current.RejectionCriterion.Add(v_RejectionCriterion);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.RejectionCriterion.Count == 0)
          {
            current.RejectionCriterion = null;
          }
          break;

        case "handling":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Handling = new List<SpecimenDefinition.HandlingComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.SpecimenDefinition.HandlingComponent v_Handling = new Hl7.Fhir.Model.SpecimenDefinition.HandlingComponent();
            v_Handling.DeserializeJson(ref reader, options);
            current.Handling.Add(v_Handling);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Handling.Count == 0)
          {
            current.Handling = null;
          }
          break;

        // Complex: typeTested, Export: TypeTestedComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR SpecimenDefinition#Container into JSON
    /// </summary>
    public static void SerializeJson(this SpecimenDefinition.ContainerComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: SpecimenDefinition#Container, Export: ContainerComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Material != null)
      {
        writer.WritePropertyName("material");
        current.Material.SerializeJson(writer, options);
      }

      if (current.Type != null)
      {
        writer.WritePropertyName("type");
        current.Type.SerializeJson(writer, options);
      }

      if (current.Cap != null)
      {
        writer.WritePropertyName("cap");
        current.Cap.SerializeJson(writer, options);
      }

      if (current.DescriptionElement != null)
      {
        if (!string.IsNullOrEmpty(current.DescriptionElement.Value))
        {
          writer.WriteString("description",current.DescriptionElement.Value);
        }
        if (current.DescriptionElement.HasExtensions() || (!string.IsNullOrEmpty(current.DescriptionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_description",false,current.DescriptionElement.Extension,current.DescriptionElement.ElementId);
        }
      }

      if (current.Capacity != null)
      {
        writer.WritePropertyName("capacity");
        current.Capacity.SerializeJson(writer, options);
      }

      if (current.MinimumVolume != null)
      {
        switch (current.MinimumVolume)
        {
          case Quantity v_Quantity:
            writer.WritePropertyName("minimumVolumeQuantity");
            v_Quantity.SerializeJson(writer, options);
            break;
          case FhirString v_FhirString:
            writer.WriteString("minimumVolumeString",v_FhirString.Value);
            break;
        }
      }
      if ((current.Additive != null) && (current.Additive.Count != 0))
      {
        writer.WritePropertyName("additive");
        writer.WriteStartArray();
        foreach (SpecimenDefinition.AdditiveComponent val in current.Additive)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.PreparationElement != null)
      {
        if (!string.IsNullOrEmpty(current.PreparationElement.Value))
        {
          writer.WriteString("preparation",current.PreparationElement.Value);
        }
        if (current.PreparationElement.HasExtensions() || (!string.IsNullOrEmpty(current.PreparationElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_preparation",false,current.PreparationElement.Extension,current.PreparationElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#Container
    /// </summary>
    public static void DeserializeJson(this SpecimenDefinition.ContainerComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#Container
    /// </summary>
    public static void DeserializeJsonProperty(this SpecimenDefinition.ContainerComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "material":
          current.Material = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Material).DeserializeJson(ref reader, options);
          break;

        case "type":
          current.Type = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Type).DeserializeJson(ref reader, options);
          break;

        case "cap":
          current.Cap = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Cap).DeserializeJson(ref reader, options);
          break;

        case "description":
          current.DescriptionElement = new FhirString(reader.GetString());
          break;

        case "_description":
          ((Hl7.Fhir.Model.Element)current.DescriptionElement).DeserializeJson(ref reader, options);
          break;

        case "capacity":
          current.Capacity = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.Capacity).DeserializeJson(ref reader, options);
          break;

        case "minimumVolumeQuantity":
          current.MinimumVolume = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.MinimumVolume).DeserializeJson(ref reader, options);
          break;

        case "minimumVolumeString":
          current.MinimumVolume = new FhirString(reader.GetString());
          break;

        case "additive":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Additive = new List<SpecimenDefinition.AdditiveComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.SpecimenDefinition.AdditiveComponent v_Additive = new Hl7.Fhir.Model.SpecimenDefinition.AdditiveComponent();
            v_Additive.DeserializeJson(ref reader, options);
            current.Additive.Add(v_Additive);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Additive.Count == 0)
          {
            current.Additive = null;
          }
          break;

        case "preparation":
          current.PreparationElement = new FhirString(reader.GetString());
          break;

        case "_preparation":
          ((Hl7.Fhir.Model.Element)current.PreparationElement).DeserializeJson(ref reader, options);
          break;

        // Complex: container, Export: ContainerComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR SpecimenDefinition#Additive into JSON
    /// </summary>
    public static void SerializeJson(this SpecimenDefinition.AdditiveComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: SpecimenDefinition#Additive, Export: AdditiveComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Additive != null)
      {
        switch (current.Additive)
        {
          case CodeableConcept v_CodeableConcept:
            writer.WritePropertyName("additiveCodeableConcept");
            v_CodeableConcept.SerializeJson(writer, options);
            break;
          case ResourceReference v_ResourceReference:
            writer.WritePropertyName("additiveReference");
            v_ResourceReference.SerializeJson(writer, options);
            break;
        }
      }
      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#Additive
    /// </summary>
    public static void DeserializeJson(this SpecimenDefinition.AdditiveComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#Additive
    /// </summary>
    public static void DeserializeJsonProperty(this SpecimenDefinition.AdditiveComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "additiveCodeableConcept":
          current.Additive = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Additive).DeserializeJson(ref reader, options);
          break;

        case "additiveReference":
          current.Additive = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Additive).DeserializeJson(ref reader, options);
          break;

        // Complex: additive, Export: AdditiveComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR SpecimenDefinition#Handling into JSON
    /// </summary>
    public static void SerializeJson(this SpecimenDefinition.HandlingComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: SpecimenDefinition#Handling, Export: HandlingComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.TemperatureQualifier != null)
      {
        writer.WritePropertyName("temperatureQualifier");
        current.TemperatureQualifier.SerializeJson(writer, options);
      }

      if (current.TemperatureRange != null)
      {
        writer.WritePropertyName("temperatureRange");
        current.TemperatureRange.SerializeJson(writer, options);
      }

      if (current.MaxDuration != null)
      {
        writer.WritePropertyName("maxDuration");
        current.MaxDuration.SerializeJson(writer, options);
      }

      if (current.InstructionElement != null)
      {
        if (!string.IsNullOrEmpty(current.InstructionElement.Value))
        {
          writer.WriteString("instruction",current.InstructionElement.Value);
        }
        if (current.InstructionElement.HasExtensions() || (!string.IsNullOrEmpty(current.InstructionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_instruction",false,current.InstructionElement.Extension,current.InstructionElement.ElementId);
        }
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#Handling
    /// </summary>
    public static void DeserializeJson(this SpecimenDefinition.HandlingComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
          current.DeserializeJsonProperty(ref reader, options, propertyName);
        }
      }

      throw new JsonException();
    }

    /// <summary>
    /// Deserialize JSON into a FHIR SpecimenDefinition#Handling
    /// </summary>
    public static void DeserializeJsonProperty(this SpecimenDefinition.HandlingComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "temperatureQualifier":
          current.TemperatureQualifier = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.TemperatureQualifier).DeserializeJson(ref reader, options);
          break;

        case "temperatureRange":
          current.TemperatureRange = new Hl7.Fhir.Model.Range();
          ((Hl7.Fhir.Model.Range)current.TemperatureRange).DeserializeJson(ref reader, options);
          break;

        case "maxDuration":
          current.MaxDuration = new Hl7.Fhir.Model.Duration();
          ((Hl7.Fhir.Model.Duration)current.MaxDuration).DeserializeJson(ref reader, options);
          break;

        case "instruction":
          current.InstructionElement = new FhirString(reader.GetString());
          break;

        case "_instruction":
          ((Hl7.Fhir.Model.Element)current.InstructionElement).DeserializeJson(ref reader, options);
          break;

        // Complex: handling, Export: HandlingComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class SpecimenDefinitionJsonConverter : JsonConverter<SpecimenDefinition>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(SpecimenDefinition).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, SpecimenDefinition value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override SpecimenDefinition Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        SpecimenDefinition target = new SpecimenDefinition();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
