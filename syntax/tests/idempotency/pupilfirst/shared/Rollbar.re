/* Rollbar.critical("Connection error from remote Payments API"); */
[@bs.val] [@bs.scope "Rollbar"] external critical: string => unit = "critical";

/* Rollbar.error("Some unexpected condition"); */
[@bs.val] [@bs.scope "Rollbar"] external error: string => unit = "error";

/* Rollbar.warning("Connection error from Twitter API"); */
[@bs.val] [@bs.scope "Rollbar"] external warning: string => unit = "warning";

/* Rollbar.info("User opened the purchase dialog"); */
[@bs.val] [@bs.scope "Rollbar"] external info: string => unit = "info";

/* Rollbar.debug("Purchase dialog finished rendering"); */
[@bs.val] [@bs.scope "Rollbar"] external debug: string => unit = "debug";
