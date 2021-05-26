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
  /// JSON Serialization Extensions for ChargeItem
  /// </summary>
  public static class ChargeItemJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR ChargeItem into JSON
    /// </summary>
    public static void SerializeJson(this ChargeItem current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","ChargeItem");
      // Complex: ChargeItem, Export: ChargeItem, Base: DomainResource (DomainResource)
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

      if ((current.DefinitionUriElement != null) && (current.DefinitionUriElement.Count != 0))
      {
        writer.WritePropertyName("definitionUri");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (FhirUri val in current.DefinitionUriElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (FhirUri val in current.DefinitionUriElement)
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
          writer.WritePropertyName("_definitionUri");
          writer.WriteStartArray();
          foreach (FhirUri val in current.DefinitionUriElement)
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

      if ((current.DefinitionCanonicalElement != null) && (current.DefinitionCanonicalElement.Count != 0))
      {
        writer.WritePropertyName("definitionCanonical");
        writer.WriteStartArray();
        bool foundExtensions = false;
        foreach (Canonical val in current.DefinitionCanonicalElement)
        {
          if (val.HasExtensions())
          {
            foundExtensions = true;
            break;
          }
        }

        foreach (Canonical val in current.DefinitionCanonicalElement)
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
          writer.WritePropertyName("_definitionCanonical");
          writer.WriteStartArray();
          foreach (Canonical val in current.DefinitionCanonicalElement)
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

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      if ((current.PartOf != null) && (current.PartOf.Count != 0))
      {
        writer.WritePropertyName("partOf");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.PartOf)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      writer.WritePropertyName("code");
      current.Code.SerializeJson(writer, options);

      writer.WritePropertyName("subject");
      current.Subject.SerializeJson(writer, options);

      if (current.Context != null)
      {
        writer.WritePropertyName("context");
        current.Context.SerializeJson(writer, options);
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
      if ((current.Performer != null) && (current.Performer.Count != 0))
      {
        writer.WritePropertyName("performer");
        writer.WriteStartArray();
        foreach (ChargeItem.PerformerComponent val in current.Performer)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.PerformingOrganization != null)
      {
        writer.WritePropertyName("performingOrganization");
        current.PerformingOrganization.SerializeJson(writer, options);
      }

      if (current.RequestingOrganization != null)
      {
        writer.WritePropertyName("requestingOrganization");
        current.RequestingOrganization.SerializeJson(writer, options);
      }

      if (current.CostCenter != null)
      {
        writer.WritePropertyName("costCenter");
        current.CostCenter.SerializeJson(writer, options);
      }

      if (current.Quantity != null)
      {
        writer.WritePropertyName("quantity");
        current.Quantity.SerializeJson(writer, options);
      }

      if ((current.Bodysite != null) && (current.Bodysite.Count != 0))
      {
        writer.WritePropertyName("bodysite");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Bodysite)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.FactorOverrideElement != null)
      {
        if (current.FactorOverrideElement.Value != null)
        {
          writer.WriteNumber("factorOverride",(decimal)current.FactorOverrideElement.Value);
        }
        if (current.FactorOverrideElement.HasExtensions() || (!string.IsNullOrEmpty(current.FactorOverrideElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_factorOverride",false,current.FactorOverrideElement.Extension,current.FactorOverrideElement.ElementId);
        }
      }

      if (current.PriceOverride != null)
      {
        writer.WritePropertyName("priceOverride");
        current.PriceOverride.SerializeJson(writer, options);
      }

      if (current.OverrideReasonElement != null)
      {
        if (!string.IsNullOrEmpty(current.OverrideReasonElement.Value))
        {
          writer.WriteString("overrideReason",current.OverrideReasonElement.Value);
        }
        if (current.OverrideReasonElement.HasExtensions() || (!string.IsNullOrEmpty(current.OverrideReasonElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_overrideReason",false,current.OverrideReasonElement.Extension,current.OverrideReasonElement.ElementId);
        }
      }

      if (current.Enterer != null)
      {
        writer.WritePropertyName("enterer");
        current.Enterer.SerializeJson(writer, options);
      }

      if (current.EnteredDateElement != null)
      {
        if (!string.IsNullOrEmpty(current.EnteredDateElement.Value))
        {
          writer.WriteString("enteredDate",current.EnteredDateElement.Value);
        }
        if (current.EnteredDateElement.HasExtensions() || (!string.IsNullOrEmpty(current.EnteredDateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_enteredDate",false,current.EnteredDateElement.Extension,current.EnteredDateElement.ElementId);
        }
      }

      if ((current.Reason != null) && (current.Reason.Count != 0))
      {
        writer.WritePropertyName("reason");
        writer.WriteStartArray();
        foreach (CodeableConcept val in current.Reason)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if ((current.Service != null) && (current.Service.Count != 0))
      {
        writer.WritePropertyName("service");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Service)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (current.Product != null)
      {
        switch (current.Product)
        {
          case ResourceReference v_ResourceReference:
            writer.WritePropertyName("productReference");
            v_ResourceReference.SerializeJson(writer, options);
            break;
          case CodeableConcept v_CodeableConcept:
            writer.WritePropertyName("productCodeableConcept");
            v_CodeableConcept.SerializeJson(writer, options);
            break;
        }
      }
      if ((current.Account != null) && (current.Account.Count != 0))
      {
        writer.WritePropertyName("account");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.Account)
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

      if ((current.SupportingInformation != null) && (current.SupportingInformation.Count != 0))
      {
        writer.WritePropertyName("supportingInformation");
        writer.WriteStartArray();
        foreach (ResourceReference val in current.SupportingInformation)
        {
          val.SerializeJson(writer, options, true);
        }
        writer.WriteEndArray();
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR ChargeItem
    /// </summary>
    public static void DeserializeJson(this ChargeItem current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR ChargeItem
    /// </summary>
    public static void DeserializeJsonProperty(this ChargeItem current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "definitionUri":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.DefinitionUriElement = new List<FhirUri>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.DefinitionUriElement.Add(new FhirUri(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.DefinitionUriElement.Count == 0)
          {
            current.DefinitionUriElement = null;
          }
          break;

        case "_definitionUri":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_definitionUri = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_definitionUri >= current.DefinitionUriElement.Count)
            {
              current.DefinitionUriElement.Add(new FhirUri());
            }
            ((Hl7.Fhir.Model.Element)current.DefinitionUriElement[i_definitionUri++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "definitionCanonical":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.DefinitionCanonicalElement = new List<Canonical>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            current.DefinitionCanonicalElement.Add(new Canonical(reader.GetString()));

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.DefinitionCanonicalElement.Count == 0)
          {
            current.DefinitionCanonicalElement = null;
          }
          break;

        case "_definitionCanonical":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          int i_definitionCanonical = 0;

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            if (i_definitionCanonical >= current.DefinitionCanonicalElement.Count)
            {
              current.DefinitionCanonicalElement.Add(new Canonical());
            }
            ((Hl7.Fhir.Model.Element)current.DefinitionCanonicalElement[i_definitionCanonical++]).DeserializeJson(ref reader, options);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }
          break;

        case "status":
          current.StatusElement =new Code<Hl7.Fhir.Model.ChargeItem.ChargeItemStatus>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.ChargeItem.ChargeItemStatus>(reader.GetString()));
          break;

        case "_status":
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "partOf":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.PartOf = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_PartOf = new Hl7.Fhir.Model.ResourceReference();
            v_PartOf.DeserializeJson(ref reader, options);
            current.PartOf.Add(v_PartOf);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.PartOf.Count == 0)
          {
            current.PartOf = null;
          }
          break;

        case "code":
          current.Code = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Code).DeserializeJson(ref reader, options);
          break;

        case "subject":
          current.Subject = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Subject).DeserializeJson(ref reader, options);
          break;

        case "context":
          current.Context = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Context).DeserializeJson(ref reader, options);
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

        case "performer":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Performer = new List<ChargeItem.PerformerComponent>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ChargeItem.PerformerComponent v_Performer = new Hl7.Fhir.Model.ChargeItem.PerformerComponent();
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

        case "performingOrganization":
          current.PerformingOrganization = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.PerformingOrganization).DeserializeJson(ref reader, options);
          break;

        case "requestingOrganization":
          current.RequestingOrganization = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.RequestingOrganization).DeserializeJson(ref reader, options);
          break;

        case "costCenter":
          current.CostCenter = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.CostCenter).DeserializeJson(ref reader, options);
          break;

        case "quantity":
          current.Quantity = new Hl7.Fhir.Model.Quantity();
          ((Hl7.Fhir.Model.Quantity)current.Quantity).DeserializeJson(ref reader, options);
          break;

        case "bodysite":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Bodysite = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Bodysite = new Hl7.Fhir.Model.CodeableConcept();
            v_Bodysite.DeserializeJson(ref reader, options);
            current.Bodysite.Add(v_Bodysite);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Bodysite.Count == 0)
          {
            current.Bodysite = null;
          }
          break;

        case "factorOverride":
          current.FactorOverrideElement = new FhirDecimal(reader.GetDecimal());
          break;

        case "_factorOverride":
          ((Hl7.Fhir.Model.Element)current.FactorOverrideElement).DeserializeJson(ref reader, options);
          break;

        case "priceOverride":
          current.PriceOverride = new Hl7.Fhir.Model.Money();
          ((Hl7.Fhir.Model.Money)current.PriceOverride).DeserializeJson(ref reader, options);
          break;

        case "overrideReason":
          current.OverrideReasonElement = new FhirString(reader.GetString());
          break;

        case "_overrideReason":
          ((Hl7.Fhir.Model.Element)current.OverrideReasonElement).DeserializeJson(ref reader, options);
          break;

        case "enterer":
          current.Enterer = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Enterer).DeserializeJson(ref reader, options);
          break;

        case "enteredDate":
          current.EnteredDateElement = new FhirDateTime(reader.GetString());
          break;

        case "_enteredDate":
          ((Hl7.Fhir.Model.Element)current.EnteredDateElement).DeserializeJson(ref reader, options);
          break;

        case "reason":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Reason = new List<CodeableConcept>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.CodeableConcept v_Reason = new Hl7.Fhir.Model.CodeableConcept();
            v_Reason.DeserializeJson(ref reader, options);
            current.Reason.Add(v_Reason);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Reason.Count == 0)
          {
            current.Reason = null;
          }
          break;

        case "service":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Service = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Service = new Hl7.Fhir.Model.ResourceReference();
            v_Service.DeserializeJson(ref reader, options);
            current.Service.Add(v_Service);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Service.Count == 0)
          {
            current.Service = null;
          }
          break;

        case "productReference":
          current.Product = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Product).DeserializeJson(ref reader, options);
          break;

        case "productCodeableConcept":
          current.Product = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Product).DeserializeJson(ref reader, options);
          break;

        case "account":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.Account = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_Account = new Hl7.Fhir.Model.ResourceReference();
            v_Account.DeserializeJson(ref reader, options);
            current.Account.Add(v_Account);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.Account.Count == 0)
          {
            current.Account = null;
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

        case "supportingInformation":
          if ((reader.TokenType != JsonTokenType.StartArray) || (!reader.Read()))
          {
            throw new JsonException();
          }

          current.SupportingInformation = new List<ResourceReference>();

          while (reader.TokenType != JsonTokenType.EndArray)
          {
            Hl7.Fhir.Model.ResourceReference v_SupportingInformation = new Hl7.Fhir.Model.ResourceReference();
            v_SupportingInformation.DeserializeJson(ref reader, options);
            current.SupportingInformation.Add(v_SupportingInformation);

            if (!reader.Read())
            {
              throw new JsonException();
            }
            if (reader.TokenType == JsonTokenType.EndObject) { reader.Read(); }
          }

          if (current.SupportingInformation.Count == 0)
          {
            current.SupportingInformation = null;
          }
          break;

        // Complex: ChargeItem, Export: ChargeItem, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Serialize a FHIR ChargeItem#Performer into JSON
    /// </summary>
    public static void SerializeJson(this ChargeItem.PerformerComponent current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      // Component: ChargeItem#Performer, Export: PerformerComponent, Base: BackboneElement (BackboneElement)
      ((Hl7.Fhir.Model.BackboneElement)current).SerializeJson(writer, options, false);

      if (current.Function != null)
      {
        writer.WritePropertyName("function");
        current.Function.SerializeJson(writer, options);
      }

      writer.WritePropertyName("actor");
      current.Actor.SerializeJson(writer, options);

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR ChargeItem#Performer
    /// </summary>
    public static void DeserializeJson(this ChargeItem.PerformerComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR ChargeItem#Performer
    /// </summary>
    public static void DeserializeJsonProperty(this ChargeItem.PerformerComponent current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
    {
      switch (propertyName)
      {
        case "function":
          current.Function = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.Function).DeserializeJson(ref reader, options);
          break;

        case "actor":
          current.Actor = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Actor).DeserializeJson(ref reader, options);
          break;

        // Complex: performer, Export: PerformerComponent, Base: BackboneElement
        default:
          ((Hl7.Fhir.Model.BackboneElement)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class ChargeItemJsonConverter : JsonConverter<ChargeItem>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(ChargeItem).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, ChargeItem value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override ChargeItem Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        ChargeItem target = new ChargeItem();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
