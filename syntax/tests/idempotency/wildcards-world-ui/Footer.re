let transparency_link = "https://blog.wildcards.world/where-is-the-money-going/";
let privacy_policy_link = "https://wildcards.world/privacy_policy.html";
let terms_and_conditions_link = "https://wildcards.world/terms_and_conditions.html";

open Globals;

let footer =
  Css.(
    style([
      backgroundColor(hex("303030")),
      height(vh(12.)),
      media("(max-width: 831px)", [height(vh(20.))]),
    ])
  );

let footerWrapper =
  Css.(
    style([
      margin(auto),
      maxWidth(px(1080)),
      display(`flex),
      justifyContent(spaceAround),
      alignItems(center),
      flexWrap(`wrap),
      height(`percent(100.)),
    ])
  );

let footerText =
  Css.(
    style([
      marginTop(auto),
      marginBottom(auto),
      color(hex("919797")),
      media(
        "(max-width: 831px)",
        [width(`percent(100.)), textAlign(`center)],
      ),
    ])
  );

let footerLink =
  Css.(style([textDecoration(none), important(color(hex("919797")))]));

let footerSocialButtons =
  Css.(
    style([
      padding(px(0)),
      media(
        "(min-width: 831px)",
        [
          marginRight(em(6.)) // This is needed to cater for the 'crisp' chat help.
        ],
      ),
      listStyle(`none, `inside, `none),
      display(`flex),
      justifyContent(center),
      alignItems(center),
      flexWrap(wrap),
      media("(max-width: 831px)", [width(`percent(100.))]),
    ])
  );

let footerSocialButton = Css.(style([]));

let footerSocailButtonLink =
  Css.(style([maxWidth(px(32)), maxHeight(px(32))]));

[@react.component]
let make = () =>
  <Rimble.Box className=footer>
    <div className=footerWrapper>
      <div className=footerText>
        <div>
          {js|©|js}->restr
          <a
            className=footerLink
            target="_blank"
            rel="noopener noreferrer"
            href="https://wildcards.world">
            "Wildcards"->restr
          </a>
        </div>
      </div>
      <div className=footerText>
        <a
          className=footerLink
          target="_blank"
          rel="noopener noreferrer"
          href=transparency_link>
          "Transparency"->restr
        </a>
      </div>
      <div className=footerText>
        <a className=footerLink rel="noopener noreferrer" href="/#team">
          "Team"->restr
        </a>
      </div>
      <div className=footerText>
        <a
          className=footerLink
          target="_blank"
          rel="noopener noreferrer"
          href=privacy_policy_link>
          "Privacy Policy"->restr
        </a>
      </div>
      <div className=footerText>
        <a
          className=footerLink
          target="_blank"
          rel="noopener noreferrer"
          href=terms_and_conditions_link>
          "Terms and Conditions"->restr
        </a>
      </div>
      <ul className=footerSocialButtons>
        <li className=footerSocialButton>
          <SocialButtons
            url="https://twitter.com/wildcards_world"
            bgColor="transparent"
            fgColor="#aaa"
            target="_blank"
            rel="noopener noreferrer"
            network="twitter"
            className=footerSocailButtonLink
          />
        </li>
        <li className=footerSocialButton>
          <SocialButtons
            className=footerSocailButtonLink
            url="https://www.facebook.com/wildcards.conservation"
            bgColor="transparent"
            target="_blank"
            rel="noopener noreferrer"
            network="facebook"
            fgColor="#aaa"
          />
        </li>
        <li className=footerSocialButton>
          <SocialButtons
            className=footerSocailButtonLink
            url="https://www.youtube.com/channel/UCW8T1lOHWs3klEJ36N9DyCA"
            bgColor="transparent"
            target="_blank"
            rel="noopener noreferrer"
            network="youtube"
            fgColor="#aaa"
          />
        </li>
        <li className=footerSocialButton>
          <SocialButtons
            className=footerSocailButtonLink
            url="https://github.com/wildcards-world"
            bgColor="transparent"
            target="_blank"
            rel="noopener noreferrer"
            network="github"
            fgColor="#aaa"
          />
        </li>
        <li className=footerSocialButton>
          <SocialButtons
            className=footerSocailButtonLink
            url="https://www.linkedin.com/company/wildcards-world/"
            bgColor="transparent"
            target="_blank"
            rel="noopener noreferrer"
            network="linkedin"
            fgColor="#aaa"
          />
        </li>
      </ul>
    </div>
  </Rimble.Box>;
