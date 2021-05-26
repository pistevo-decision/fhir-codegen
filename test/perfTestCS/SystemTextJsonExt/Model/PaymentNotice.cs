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
  /// JSON Serialization Extensions for PaymentNotice
  /// </summary>
  public static class PaymentNoticeJsonExtensions
  {
    /// <summary>
    /// Serialize a FHIR PaymentNotice into JSON
    /// </summary>
    public static void SerializeJson(this PaymentNotice current, Utf8JsonWriter writer, JsonSerializerOptions options, bool includeStartObject = true)
    {
      if (includeStartObject) { writer.WriteStartObject(); }
      writer.WriteString("resourceType","PaymentNotice");
      // Complex: PaymentNotice, Export: PaymentNotice, Base: DomainResource (DomainResource)
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

      writer.WriteString("status",Hl7.Fhir.Utility.EnumUtility.GetLiteral(current.StatusElement.Value));

      if (current.Request != null)
      {
        writer.WritePropertyName("request");
        current.Request.SerializeJson(writer, options);
      }

      if (current.Response != null)
      {
        writer.WritePropertyName("response");
        current.Response.SerializeJson(writer, options);
      }

      writer.WriteString("created",current.CreatedElement.Value);

      if (current.Provider != null)
      {
        writer.WritePropertyName("provider");
        current.Provider.SerializeJson(writer, options);
      }

      writer.WritePropertyName("payment");
      current.Payment.SerializeJson(writer, options);

      if (current.PaymentDateElement != null)
      {
        if (!string.IsNullOrEmpty(current.PaymentDateElement.Value))
        {
          writer.WriteString("paymentDate",current.PaymentDateElement.Value);
        }
        if (current.PaymentDateElement.HasExtensions() || (!string.IsNullOrEmpty(current.PaymentDateElement.ElementId)))
        {
          JsonStreamUtilities.SerializeExtensionList(writer,options,"_paymentDate",false,current.PaymentDateElement.Extension,current.PaymentDateElement.ElementId);
        }
      }

      if (current.Payee != null)
      {
        writer.WritePropertyName("payee");
        current.Payee.SerializeJson(writer, options);
      }

      writer.WritePropertyName("recipient");
      current.Recipient.SerializeJson(writer, options);

      writer.WritePropertyName("amount");
      current.Amount.SerializeJson(writer, options);

      if (current.PaymentStatus != null)
      {
        writer.WritePropertyName("paymentStatus");
        current.PaymentStatus.SerializeJson(writer, options);
      }

      if (includeStartObject) { writer.WriteEndObject(); }
    }

    /// <summary>
    /// Deserialize JSON into a FHIR PaymentNotice
    /// </summary>
    public static void DeserializeJson(this PaymentNotice current, ref Utf8JsonReader reader, JsonSerializerOptions options)
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
    /// Deserialize JSON into a FHIR PaymentNotice
    /// </summary>
    public static void DeserializeJsonProperty(this PaymentNotice current, ref Utf8JsonReader reader, JsonSerializerOptions options, string propertyName)
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

        case "status":
          current.StatusElement =new Code<Hl7.Fhir.Model.FinancialResourceStatusCodes>(Hl7.Fhir.Utility.EnumUtility.ParseLiteral<Hl7.Fhir.Model.FinancialResourceStatusCodes>(reader.GetString()));
          break;

        case "_status":
          ((Hl7.Fhir.Model.Element)current.StatusElement).DeserializeJson(ref reader, options);
          break;

        case "request":
          current.Request = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Request).DeserializeJson(ref reader, options);
          break;

        case "response":
          current.Response = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Response).DeserializeJson(ref reader, options);
          break;

        case "created":
          current.CreatedElement = new FhirDateTime(reader.GetString());
          break;

        case "_created":
          ((Hl7.Fhir.Model.Element)current.CreatedElement).DeserializeJson(ref reader, options);
          break;

        case "provider":
          current.Provider = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Provider).DeserializeJson(ref reader, options);
          break;

        case "payment":
          current.Payment = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Payment).DeserializeJson(ref reader, options);
          break;

        case "paymentDate":
          current.PaymentDateElement = new Date(reader.GetString());
          break;

        case "_paymentDate":
          ((Hl7.Fhir.Model.Element)current.PaymentDateElement).DeserializeJson(ref reader, options);
          break;

        case "payee":
          current.Payee = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Payee).DeserializeJson(ref reader, options);
          break;

        case "recipient":
          current.Recipient = new Hl7.Fhir.Model.ResourceReference();
          ((Hl7.Fhir.Model.ResourceReference)current.Recipient).DeserializeJson(ref reader, options);
          break;

        case "amount":
          current.Amount = new Hl7.Fhir.Model.Money();
          ((Hl7.Fhir.Model.Money)current.Amount).DeserializeJson(ref reader, options);
          break;

        case "paymentStatus":
          current.PaymentStatus = new Hl7.Fhir.Model.CodeableConcept();
          ((Hl7.Fhir.Model.CodeableConcept)current.PaymentStatus).DeserializeJson(ref reader, options);
          break;

        // Complex: PaymentNotice, Export: PaymentNotice, Base: DomainResource
        default:
          ((Hl7.Fhir.Model.DomainResource)current).DeserializeJsonProperty(ref reader, options, propertyName);
          break;
      }
    }

    /// <summary>
    /// Resource converter to support Sytem.Text.Json interop.
    /// </summary>
    public class PaymentNoticeJsonConverter : JsonConverter<PaymentNotice>
    {
      /// <summary>
      /// Determines whether the specified type can be converted.
      /// </summary>
      public override bool CanConvert(Type objectType) =>
        typeof(PaymentNotice).IsAssignableFrom(objectType);

      /// <summary>
      /// Writes a specified value as JSON.
      /// </summary>
      public override void Write(Utf8JsonWriter writer, PaymentNotice value, JsonSerializerOptions options)
      {
        value.SerializeJson(writer, options, true);
        writer.Flush();
      }
      /// <summary>
      /// Reads and converts the JSON to a typed object.
      /// </summary>
      public override PaymentNotice Read(ref Utf8JsonReader reader, Type typeToConvert, JsonSerializerOptions options)
      {
        PaymentNotice target = new PaymentNotice();
        target.DeserializeJson(ref reader, options);
        return target;
      }
    }
  }

}

// end of file
