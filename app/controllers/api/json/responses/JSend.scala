package controllers.api.json.responses

import play.api.libs.json.Json
import play.api.libs.json.JsValue

object JSend {
    def success(res: JsValue): JsValue = {
        Json.obj(
            "status" -> "success",
            "data" -> res
        )
    }
    
    def fail(res: JsValue): JsValue = {
        Json.obj(
            "status" -> "fail",
            "data" -> res
        )
    }
    
    def error(msg: String): JsValue = {
        Json.obj(
            "status" -> "error",
            "message" -> Json.toJson(msg)
        )
    }
}