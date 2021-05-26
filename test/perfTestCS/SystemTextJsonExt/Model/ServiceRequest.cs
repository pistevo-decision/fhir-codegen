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
  /// JSON Serialization Extensions for ServiceRequest
  /// </summary>
  public static class ServiceRequestJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR ServiceRequest into JSON
    /// </summary>
    public static void SerializeJson(this ServiceRequest current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","ServiceRequest");
      // Complex: ServiceRequest, Export: ServiceRequest, Base: DomainResource (DomainResource)
      ((Hl7.Fhir.Model.DomainResource)current).SerializeJson(writer, options, false);

      if ((current.Identifier != null) && (current.Identifier.Count != 0))
      {
        writer.WritePropertyName("identifier");
        writer.WriteStartArray();
        foreach (Identifier val in current.Identifier)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.InstantiatesCanonicalElement != null) && (current.InstantiatesCanonicalElement.Count != 0))
      {
        writer.WritePropertyName("instantiatesCanonical");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (Canonical val in current.InstantiatesCanonicalElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (Canonical val in current.InstantiatesCanonicalElement)
        {
          if (string.IsNullOrEmpty(val.Value))
          {
            if (foundExtensions) { writer.WriteNullValue(); }
          }
          else
          {
            writer.WriteStringValue(val.Value);
          }

        }
        if (foundExtensions)
        {
          writer.WriteEndArray();
          writer.WritePropertyName("_instantiatesCanonical");
          writer.WriteStartArray();
          foreach (Canonical val in current.InstantiatesCanonicalElement)
          {
            if (val.HasExtensions() || (!string.IsNullOrEmpty(val.ElementId)))
            {
              JsonStreamUtilities.SerializeExtensionList(writer,options,string.Empty,true,val.Extension,val.ElementId);
            }
            else
            {
              writer.WriteNullValue();
            }

          }
        }
        writer.WriteEndArray();
      }

      if ((current.InstantiatesUriElement != null) && (current.InstantiatesUriElement.Count != 0))
      {
        writer.WritePropertyName("instantiatesUri");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (FhirUri val in current.InstantiatesUriElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (FhirUri val in current.InstantiatesUriElement)
        {
          if (string.IsNullOrEmpty(val.Value))
          {
            if (foundExtensions) { writer.WriteNullValue(); }
          }
          else
          {
            writer.WriteStringValue(val.Value);
          }

        }
        if (foundExtensions)
        {
          writer.WriteEndArray();
          writer.WritePropertyName("_instantiatesUri");
          writer.WriteStartArray();
          foreach (FhirUri val in current.InstantiatesUriElement)
          {
            if (val.HasExtensions() || (!string.IsNullOrEmpty(val.ElementId)))
            {
              JsonStreamUtilities.SerializeExtensionList(writer,options,string.Empty,true,val.Extension,val.ElementId);
            }
            else
            {
              writer.WriteNullValue();
            }

          }
        }
        writer.WriteEndArray();
      }

      if ((current.BasedOn != null) && (current.BasedOn.Count != 0))
      {
        writer.WritePropertyName("basedOn");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.BasedOn)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Replaces != null) && (current.Replaces.Count != 0))
      {
        writer.WritePropertyName("replaces");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Replaces)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Requisition != null)
      {
        writer.WritePropertyName("requisition");
        current.Requisition.SerializeJson(writer, options);
      }

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      writer.WriteString("intent",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.IntentElement.Value));

      if ((current.Category != null) && (current.Category.Count != 0))
      {
        writer.WritePropertyName("category");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Category)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.PriorityElement != null)
      {
        writer.WriteString("priority",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.PriorityElement.Value));
      }

      if (current.DoNotPerformElement != null)
      {
        if (current.DoNotPerformElement.Value != null)
        {
          writer.WriteBoolean("doNotPerform",(bool)current.DoNotPerformElement.Value);
        }
        if (current.DoNotPerformElement.HasExtensions() || (!string.IsNullOrEmpty(current.DoNotPerformElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_doNotPerform",false,current.DoNotPerformElement.Extension,current.DoNotPerformElement.ElementId);
        }
      }

      if (current.Code != null)
      {
        writer.WritePropertyName("code");
        current.Code.SerializeJson(writer, options);
      }

      if ((current.OrderDetail != null) && (current.OrderDetail.Count != 0))
      {
        writer.WritePropertyName("orderDetail");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.OrderDetail)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Quantity != null)
      {
        switch (current.Quantity)
        {
          case Quantity v_Quantity:
            writer.WritePropertyName("quantityQuantity");
            v_Quantity.SerializeJson(writer, options);
            break;
          case Ratio v_Ratio:
            writer.WritePropertyName("quantityRatio");
            v_Ratio.SerializeJson(writer, options);
            break;
          case Range v_Range:
            writer.WritePropertyName("quantityRange");
            v_Range.SerializeJson(writer, options);
            break;
        }
      }
      writer.WritePropertyName("subject");
      current.Subject.SerializeJson(writer, options);

      if (current.Encounter != null)
      {
        writer.WritePropertyName("encounter");
        current.Encounter.SerializeJson(writer, options);
      }

      if (current.Occurrence != null)
      {
        switch (current.Occurrence)
        {
          case FhirDateTime v_FhirDateTime:
            writer.WriteString("occurrenceDateTime",v_FhirDateTime.Value);
            break;
          case Period v_Period:
            writer.WritePropertyName("occurrencePeriod");
            v_Period.SerializeJson(writer, options);
            break;
          case Timing v_Timing:
            writer.WritePropertyName("occurrenceTiming");
            v_Timing.SerializeJson(writer, options);
            break;
        }
      }
      if (current.AsNeeded != null)
      {
        switch (current.AsNeeded)
        {
          case FhirBoolean v_FhirBoolean:
            writer.WriteBoolean("asNeededBoolean", (bool)v_FhirBoolean.Value);
            break;
          case CodeableConcept v_CodeableConcept:
            writer.WritePropertyName("asNeededCodeableConcept");
            v_CodeableConcept.SerializeJson(writer, options);
            break;
        }
      }
      if (current.AuthoredOnElement != null)
      {
        if (!string.IsNullOrEmpty(current.AuthoredOnElement.Value))
        {
          writer.WriteString("authoredOn",current.AuthoredOnElement.Value);
        }
        if (current.AuthoredOnElement.HasExtensions() || (!string.IsNullOrEmpty(current.AuthoredOnElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_authoredOn",false,current.AuthoredOnElement.Extension,current.AuthoredOnElement.ElementId);
        }
      }

      if (current.Requester != null)
      {
        writer.WritePropertyName("requester");
        current.Requester.SerializeJson(writer, options);
      }

      if (current.PerformerType != null)
      {
        writer.WritePropertyName("performerType");
        current.PerformerType.SerializeJson(writer, options);
      }

      if ((current.Performer != null) && (current.Performer.Count != 0))
      {
        writer.WritePropertyName("performer");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Performer)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.LocationCode != null) && (current.LocationCode.Count != 0))
      {
        writer.WritePropertyName("locationCode");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.LocationCode)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.LocationReference != null) && (current.LocationReference.Count != 0))
      {
        writer.WritePropertyName("locationReference");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.LocationReference)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.ReasonCode != null) && (current.ReasonCode.Count != 0))
      {
        writer.WritePropertyName("reasonCode");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.ReasonCode)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.ReasonReference != null) && (current.ReasonReference.Count != 0))
      {
        writer.WritePropertyName("reasonReference");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.ReasonReference)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Insurance != null) && (current.Insurance.Count != 0))
      {
        writer.WritePropertyName("insurance");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Insurance)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.SupportingInfo != null) && (current.SupportingInfo.Count != 0))
      {
        writer.WritePropertyName("supportingInfo");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.SupportingInfo)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Specimen != null) && (current.Specimen.Count != 0))
      {
        writer.WritePropertyName("specimen");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Specimen)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.BodySite != null) && (current.BodySite.Count != 0))
      {
        writer.WritePropertyName("bodySite");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.BodySite)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Note != null) && (current.Note.Count != 0))
      {
        writer.WritePropertyName("note");
        writer.WriteStartArray();
        foreach (Annotation val in current.Note)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.PatientInstructionElement != null)
      {
        if (!string.IsNullOrEmpty(current.PatientInstructionElement.Value))
        {
          writer.WriteString("patientInstruction",current.PatientInstructionElement.Value);
        }
        if (current.PatientInstructionElement.HasExtensions() || (!string.IsNullOrEmpty(current.PatientInstructionElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_patientInstruction",false,current.PatientInstructionElement.Extension,current.PatientInstructionElement.ElementId);
        }
      }

      if ((current.RelevantHistory != null) && (current.RelevantHistory.Count != 0))
      {
        writer.WritePropertyName("relevantHistory");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.RelevantHistory)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR ServiceRequest
    /// </summary>
    public static void DeserializeJson(this ServiceRequest current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR ServiceRequest
    /// </summary>
    public static void DeserializeJsonProperty(this ServiceRequest current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "identifier":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Identifier = new List<Identifier>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Identifier v_Identifier = new Hl7.Fhir.Model.Identifier();
            v_Identifier.DeserializeJson(ref reader, options);
            current.Identifier.Add(v_Identifier);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Identifier.Count == 0)
          {
            current.Identifier = null;
          }
          break;

        case "instantiatesCanonical":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.InstantiatesCanonicalElement = new List<Canonical>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.InstantiatesCanonicalElement.Add(new Canonical(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.InstantiatesCanonicalElement.Count == 0)
          {
            current.InstantiatesCanonicalElement = null;
          }
          break;

        case "_instantiatesCanonical":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_instantiatesCanonical = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_instantiatesCanonical >= current.InstantiatesCanonicalElement.Count)
            {
              current.InstantiatesCanonicalElement.Add(new Canonical());
            }
            ((Hl7.Fhir.Model.Element)current.InstantiatesCanonicalElement[i_instantiatesCanonical++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "instantiatesUri":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.InstantiatesUriElement = new List<FhirUri>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.InstantiatesUriElement.Add(new FhirUri(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.InstantiatesUriElement.Count == 0)
          {
            current.InstantiatesUriElement = null;
          }
          break;

        case "_instantiatesUri":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_instantiatesUri = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_instantiatesUri >= current.InstantiatesUriElement.Count)
            {
              current.InstantiatesUriElement.Add(new FhirUri());
            }
            ((Hl7.Fhir.Model.Element)current.InstantiatesUriElement[i_instantiatesUri++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "basedOn":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.BasedOn = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_BasedOn = new Hl7.Fhir.Model.ResourceReference();
            v_BasedOn.DeserializeJson(ref reader, options);
            current.BasedOn.Add(v_BasedOn);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.BasedOn.Count == 0)
          {
            current.BasedOn = null;
          }
          break;

        case "replaces":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Replaces = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Replaces = new Hl7.Fhir.Model.ResourceReference();
            v_Replaces.DeserializeJson(ref reader, options);
            current.Replaces.Add(v_Replaces);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Replaces.Count == 0)
          {
            current.Replaces = null;
          }
          break;

        case "requisition":
          current.Requisition = new Hl7.Fhir.Model.Identifier();
          ((Hl7.Fhir.Model.Identifier)current.Requisition).DeserializeJson(ref reader, options);
          break;

        case "status":
          current.StatusElement =new Code<Hl7.Fhir.Model.RequestStatus>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.RequestStatus>(reader.GetString()));
          break;

        case "_status":
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "intent":
          current.IntentElement =new Code<Hl7.Fhir.Model.RequestIntent>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.RequestIntent>(reader.GetString()));
          break;

        case "_intent":
          ((Hl7.Fhir.Model.Element)current.IntentElement).DeserializeJson(ref reader, options);
          break;

        case "category":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Category = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Category = new Hl7.Fhir.Model.CodeableConcept();
            v_Category.DeserializeJson(ref reader, options);
            current.Category.Add(v_Category);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Category.Count == 0)
          {
            current.Category = null;
          }
          break;

        case "priority":
          current.PriorityElement =new Code<Hl7.Fhir.Model.RequestPriority>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.RequestPriority>(reader.GetString()));
          break;

        case "_priority":
          ((Hl7.Fhir.Model.Element)current.PriorityElement).DeserializeJson(ref reader, options);
          break;

        case "doNotPerform":
          current.DoNotPerformElement = new FhirBoolean(reader.GetBoolean());
          break;

        case "_doNotPerform":
          ((Hl7.Fhir.Model.Element)current.DoNotPerformElement).DeserializeJson(ref reader, options);
          break;

        case "code":
          current.Code = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Code).DeserializeJson(ref reader, options);
          break;

        case "orderDetail":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.OrderDetail = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_OrderDetail = new Hl7.Fhir.Model.CodeableConcept();
            v_OrderDetail.DeserializeJson(ref reader, options);
            current.OrderDetail.Add(v_OrderDetail);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.OrderDetail.Count == 0)
          {
            current.OrderDetail = null;
          }
          break;

        case "quantityQuantity":
          current.Quantity = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.Quantity).DeserializeJson(ref reader, options);
          break;

        case "quantityRatio":
          current.Quantity = new Hl7.Fhir.Model.Ratio();
          ((Hl7.Fhir.Model.Ratio)current.Quantity).DeserializeJson(ref reader, options);
          break;

        case "quantityRange":
          current.Quantity = new Hl7.Fhir.Model.Range();
          ((Hl7.Fhir.Model.Range)current.Quantity).DeserializeJson(ref reader, options);
          break;

        case "subject":
          current.Subject = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Subject).DeserializeJson(ref reader, options);
          break;

        case "encounter":
          current.Encounter = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Encounter).DeserializeJson(ref reader, options);
          break;

        case "occurrenceDateTime":
          current.Occurrence = new FhirDateTime(reader.GetString());
          break;

        case "occurrencePeriod":
          current.Occurrence = new Hl7.Fhir.Model.Period();
          ((Hl7.Fhir.Model.Period)current.Occurrence).DeserializeJson(ref reader, options);
          break;

        case "occurrenceTiming":
          current.Occurrence = new Hl7.Fhir.Model.Timing();
          ((Hl7.Fhir.Model.Timing)current.Occurrence).DeserializeJson(ref reader, options);
          break;

        case "asNeededBoolean":
          current.AsNeeded = new FhirBoolean(reader.GetBoolean());
          break;

        case "asNeededCodeableConcept":
          current.AsNeeded = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.AsNeeded).DeserializeJson(ref reader, options);
          break;

        case "authoredOn":
          current.AuthoredOnElement = new FhirDateTime(reader.GetString());
          break;

        case "_authoredOn":
          ((Hl7.Fhir.Model.Element)current.AuthoredOnElement).DeserializeJson(ref reader, options);
          break;

        case "requester":
          current.Requester = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Requester).DeserializeJson(ref reader, options);
          break;

        case "performerType":
          current.PerformerType = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.PerformerType).DeserializeJson(ref reader, options);
          break;

        case "performer":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Performer = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Performer = new Hl7.Fhir.Model.ResourceReference();
            v_Performer.DeserializeJson(ref reader, options);
            current.Performer.Add(v_Performer);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Performer.Count == 0)
          {
            current.Performer = null;
          }
          break;

        case "locationCode":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.LocationCode = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_LocationCode = new Hl7.Fhir.Model.CodeableConcept();
            v_LocationCode.DeserializeJson(ref reader, options);
            current.LocationCode.Add(v_LocationCode);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.LocationCode.Count == 0)
          {
            current.LocationCode = null;
          }
          break;

        case "locationReference":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.LocationReference = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_LocationReference = new Hl7.Fhir.Model.ResourceReference();
            v_LocationReference.DeserializeJson(ref reader, options);
            current.LocationReference.Add(v_LocationReference);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.LocationReference.Count == 0)
          {
            current.LocationReference = null;
          }
          break;

        case "reasonCode":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.ReasonCode = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_ReasonCode = new Hl7.Fhir.Model.CodeableConcept();
            v_ReasonCode.DeserializeJson(ref reader, options);
            current.ReasonCode.Add(v_ReasonCode);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.ReasonCode.Count == 0)
          {
            current.ReasonCode = null;
          }
          break;

        case "reasonReference":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.ReasonReference = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_ReasonReference = new Hl7.Fhir.Model.ResourceReference();
            v_ReasonReference.DeserializeJson(ref reader, options);
            current.ReasonReference.Add(v_ReasonReference);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.ReasonReference.Count == 0)
          {
            current.ReasonReference = null;
          }
          break;

        case "insurance":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Insurance = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Insurance = new Hl7.Fhir.Model.ResourceReference();
            v_Insurance.DeserializeJson(ref reader, options);
            current.Insurance.Add(v_Insurance);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Insurance.Count == 0)
          {
            current.Insurance = null;
          }
          break;

        case "supportingInfo":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.SupportingInfo = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_SupportingInfo = new Hl7.Fhir.Model.ResourceReference();
            v_SupportingInfo.DeserializeJson(ref reader, options);
            current.SupportingInfo.Add(v_SupportingInfo);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.SupportingInfo.Count == 0)
          {
            current.SupportingInfo = null;
          }
          break;

        case "specimen":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Specimen = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Specimen = new Hl7.Fhir.Model.ResourceReference();
            v_Specimen.DeserializeJson(ref reader, options);
            current.Specimen.Add(v_Specimen);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Specimen.Count == 0)
          {
            current.Specimen = null;
          }
          break;

        case "bodySite":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.BodySite = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_BodySite = new Hl7.Fhir.Model.CodeableConcept();
            v_BodySite.DeserializeJson(ref reader, options);
            current.BodySite.Add(v_BodySite);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.BodySite.Count == 0)
          {
            current.BodySite = null;
          }
          break;

        case "note":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Note = new List<Annotation>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.Annotation v_Note = new Hl7.Fhir.Model.Annotation();
            v_Note.DeserializeJson(ref reader, options);
            current.Note.Add(v_Note);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Note.Count == 0)
          {
            current.Note = null;
          }
          break;

        case "patientInstruction":
          current.PatientInstructionElement = new FhirString(reader.GetString());
          break;

        case "_patientInstruction":
          ((Hl7.Fhir.Model.Element)current.PatientInstructionElement).DeserializeJson(ref reader, options);
          break;

        case "relevantHistory":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.RelevantHistory = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_RelevantHistory = new Hl7.Fhir.Model.ResourceReference();
            v_RelevantHistory.DeserializeJson(ref reader, options);
            current.RelevantHistory.Add(v_RelevantHistory);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.RelevantHistory.Count == 0)
          {
            current.RelevantHistory = null;
          }
          break;

        // Complex: ServiceRequest, Export: ServiceRequest, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class ServiceRequestJsonConverter : JsonConverter<ServiceRequest>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(ServiceRequest).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, ServiceRequest value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override ServiceRequest Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        ServiceRequest target = new ServiceRequest();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
